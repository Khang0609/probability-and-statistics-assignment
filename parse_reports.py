import os
import re
import csv
from pathlib import Path

def parse_timing_summary(timing_file_path):
    """
    Parse Timing Summary report to get Period (ns) and WNS (ns).
    Calculates Delay = Period - WNS.
    """
    with open(timing_file_path, "r", encoding="utf-8", errors="ignore") as f:
        content = f.read()

    # 1. Trích xuất WNS từ bảng Design Timing Summary
    # Dạng bảng:
    #     WNS(ns)      TNS(ns)  ...
    #     -------      -------  ...
    #       2.845        0.000  ...
    wns = None
    design_timing_match = re.search(
        r"\|\s*Design Timing Summary\s*\|.*?"
        r"WNS\(ns\)[^\n]*\n"
        r"\s*[- ]+\n"
        r"\s*(-?[\d\.]+)",
        content,
        re.DOTALL
    )
    if design_timing_match:
        wns = float(design_timing_match.group(1))
    else:
        # Fallback regex tìm dòng WNS bên dưới dòng gạch ngang
        fallback_match = re.search(r"WNS\(ns\).*?\n\s*[-]+\s*\n\s*(-?[\d\.]+)", content)
        if fallback_match:
            wns = float(fallback_match.group(1))

    # 2. Trích xuất Period từ Clock Summary
    # Dạng bảng:
    # Clock  Waveform(ns)       Period(ns)      Frequency(MHz)
    # -----  ------------       ----------      --------------
    # clk    {0.000 5.000}      10.000          100.000
    period = None
    clock_match = re.search(
        r"Clock\s+Waveform\(ns\)\s+Period\(ns\).*?\n\s*[- ]+\n\s*\w+\s+\{[^\}]+\}\s+([\d\.]+)",
        content
    )
    if clock_match:
        period = float(clock_match.group(1))
    else:
        # Fallback nếu pattern khác một chút
        fallback_clk = re.search(r"\bclk\b.*?\{.*?\b([\d\.]+)\b", content)
        if fallback_clk:
            period = float(fallback_clk.group(1))

    if wns is None or period is None:
        raise ValueError(f"Không thể trích xuất đầy đủ WNS ({wns}) hoặc Period ({period}) từ {timing_file_path}")

    delay = period - wns
    return period, wns, delay

def parse_power_file(power_file_path, delay_ns):
    """
    Parse a single power report .rpt file.
    Trích xuất:
    - Dynamic (W) -> mW
    - Device Static (W) -> mW
    - Total on chip (mW) = Dynamic + Static
    - Junction Temperature (C)
    - Simulation Activity File -> Filename & BatchID
    - Dynamic energy (pJ) = Dynamic (mW) * Delay (ns)
    """
    with open(power_file_path, "r", encoding="utf-8", errors="ignore") as f:
        content = f.read()

    # Dynamic (W)
    dynamic_match = re.search(r"\|\s*Dynamic\s*\(W\)\s*\|\s*([\d\.\-eE]+)", content)
    # Device Static (W)
    static_match = re.search(r"\|\s*Device\s*Static\s*\(W\)\s*\|\s*([\d\.\-eE]+)", content)
    # Junction Temperature (C)
    temp_match = re.search(r"\|\s*Junction\s*Temperature\s*\(C\)\s*\|\s*([\d\.\-eE]+)", content)
    # Simulation Activity File
    saif_match = re.search(r"\|\s*Simulation\s*Activity\s*File\s*\|\s*([^\|\n]+)", content)

    if not (dynamic_match and static_match and temp_match and saif_match):
        return None

    dynamic_w = float(dynamic_match.group(1).strip())
    static_w = float(static_match.group(1).strip())
    junction_temp = float(temp_match.group(1).strip())
    raw_saif_path = saif_match.group(1).strip()

    # Lấy tên file cuối cùng
    saif_filename = Path(raw_saif_path).name

    # Lấy BatchID từ tên file (ví dụ: batch_01.saif -> batch_01 hoặc lấy số 01)
    batch_id_match = re.search(r"(batch_\d+|\d+)", saif_filename, re.IGNORECASE)
    batch_id = batch_id_match.group(1) if batch_id_match else Path(saif_filename).stem

    # Đổi đơn vị sang mW
    dynamic_mw = dynamic_w * 1000.0
    static_mw = static_w * 1000.0
    total_on_chip_mw = dynamic_mw + static_mw

    # Dynamic energy (pJ)
    # 1 mW * 1 ns = 10^-3 W * 10^-9 s = 10^-12 J = 1 pJ
    dynamic_energy_pj = dynamic_mw * delay_ns

    return {
        "Filename": saif_filename,
        "BatchID": batch_id,
        "Dynamic_power_mW": round(dynamic_mw, 4),
        "Static_power_mW": round(static_mw, 4),
        "Total_on_chip_mW": round(total_on_chip_mw, 4),
        "Junction_temp_C": round(junction_temp, 2),
        "Dynamic_energy_pJ": round(dynamic_energy_pj, 6),
    }

def process_folder(folder_path):
    folder = Path(folder_path)
    if not folder.exists() or not folder.is_dir():
        print(f"[SKIP] Thư mục không tồn tại: {folder}")
        return

    print(f"\n--- Đang xử lý thư mục: {folder.name} ---")

    # 1. Tìm file timing summary
    timing_files = list(folder.glob("*[Tt]iming_[Ss]ummary.rpt")) + list(folder.glob("timing_summary.rpt"))
    if not timing_files:
        print(f"[CẢNH BÁO] Không tìm thấy file timing summary trong {folder.name}. Bỏ qua.")
        return

    timing_file = timing_files[0]
    print(f"File timing: {timing_file.name}")
    try:
        period, wns, delay = parse_timing_summary(timing_file)
        print(f"  -> Period: {period} ns, WNS: {wns} ns, Delay: {delay:.4f} ns")
    except Exception as e:
        print(f"[LỖI] Không thể đọc timing: {e}")
        return

    # 2. Tìm tất cả các file power report (.rpt)
    # Loại trừ file timing
    power_files = [
        f for f in folder.glob("*.rpt")
        if f.resolve() != timing_file.resolve() and "timing" not in f.name.lower()
    ]

    records = []
    for p_file in power_files:
        try:
            data = parse_power_file(p_file, delay)
            if data:
                records.append(data)
        except Exception as e:
            print(f"Lỗi khi đọc file {p_file.name}: {e}")

    if not records:
        print(f"[CẢNH BÁO] Không có dữ liệu power hợp lệ trong {folder.name}.")
        return

    # Sắp xếp theo BatchID (ưu tiên số nguyên nếu có số)
    def sort_key(rec):
        match = re.search(r"\d+", rec["BatchID"])
        return int(match.group(0)) if match else rec["BatchID"]

    records.sort(key=sort_key)

    # 3. Ghi vào file CSV tại từng folder
    csv_path = folder / f"{folder.name}_power_summary.csv"
    fieldnames = [
        "Filename",
        "BatchID",
        "Dynamic_power_mW",
        "Static_power_mW",
        "Total_on_chip_mW",
        "Junction_temp_C",
        "Dynamic_energy_pJ",
    ]

    with open(csv_path, "w", newline="", encoding="utf-8") as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(records)

    print(f"[THÀNH CÔNG] Đã lưu {len(records)} dòng vào: {csv_path.resolve()}")

def main():
    base_dir = Path(__file__).resolve().parent

    # Theo yêu cầu: Mở các folder (Array, Booth), bỏ qua Vedict do thiếu thông số timing
    target_folders = ["Array", "Booth"]

    for folder_name in target_folders:
        folder_path = base_dir / folder_name
        process_folder(folder_path)

if __name__ == "__main__":
    main()

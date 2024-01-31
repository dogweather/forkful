---
title:                "Làm việc với CSV"
date:                  2024-01-28T22:10:15.803832-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm việc với CSV trong PowerShell

## Cái gì & Tại sao?

Làm việc với CSV (Comma-Separated Values) liên quan đến việc xử lý dữ liệu dạng văn bản được phân tách bởi dấu phẩy thành các hàng và cột. Lập trình viên làm việc với CSV để trao đổi dữ liệu giữa các chương trình và hệ thống do sự đơn giản và sự hỗ trợ rộng rãi của nó.

## Cách thực hiện:

### Nhập một tệp CSV
```PowerShell
$data = Import-Csv -Path "path\to\yourfile.csv"
$data
```
**Đầu ra mẫu:**
```
Tên         Nghề nghiệp    Địa điểm
----        ----------    --------
John Doe    Nhà phát triển New York
Jane Smith  Nhà phân tích San Francisco
```

### Xuất ra tệp CSV
```PowerShell
$data | Export-Csv -Path "path\to\newfile.csv" -NoTypeInformation
```
**Tạo "newfile.csv" với dữ liệu từ `$data`.**

### Thêm một hàng vào dữ liệu CSV
```PowerShell
$newRow = [PSCustomObject]@{
    Name       = 'Emily Clark'
    Occupation = 'Nhà thiết kế'
    Location   = 'Austin'
}
$data += $newRow
$data | Export-Csv -Path "path\to\yourfile.csv" -NoTypeInformation
```

### Chọn các cột cụ thể
```PowerShell
$data | Select-Object Name, Location
```
**Đầu ra mẫu:**
```
Tên         Địa điểm
----        --------
John Doe    New York
Jane Smith  San Francisco
Emily Clark Austin
```

## Phân tích sâu

Lịch sử, các tệp CSV có gốc rễ từ thời kỳ đầu của máy tính như một cách đơn giản để tổ chức dữ liệu bảng mà không cần các định dạng tệp phức tạp. Các lựa chọn thay thế, như XML và JSON, cung cấp các cấu trúc dữ liệu phong phú hơn, nhưng CSV nổi bật cho dữ liệu dạng bảng do tính dễ đọc, chi phí thấp và dễ chỉnh sửa với các trình soạn thảo văn bản đơn giản. Trong PowerShell, các cmdlet `Import-Csv` và `Export-Csv` đóng gói các chi tiết triển khai, xử lý IO tệp và chuyển đổi dữ liệu thành và từ các đối tượng .NET.

## Xem Thêm

- [Tài liệu PowerShell về Import-Csv](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv)
- [Tài liệu PowerShell về Export-Csv](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/export-csv)

---
title:                "Đọc các đối số dòng lệnh"
date:                  2024-01-28T22:05:44.888110-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc các đối số dòng lệnh"

category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Việc đọc các đối số dòng lệnh cho phép các kịch bản (scripts) thay đổi cách thức hoạt động dựa trên các đầu vào từ bên ngoài mã nguồn. Lập trình viên sử dụng chúng vì chúng làm cho kịch bản linh hoạt, có thể sử dụng trong nhiều tình huống mà không cần chỉnh sửa mã nguồn.

## Làm thế nào

PowerShell đọc các đối số dòng lệnh bằng cách sử dụng mảng `$args` hoặc các tham số. `$args` nhanh cho các kịch bản sử dụng một lần; các tham số tốt hơn cho các công cụ mạnh mẽ.

### Sử dụng `$args`
```PowerShell
# myscript.ps1
Write-Host "Bạn đã nhập các đối số sau:"
$args
```
Chạy với `.\myscript.ps1 Hello PowerShell`, xuất ra:
```
Bạn đã nhập các đối số sau:
Hello PowerShell
```

### Sử dụng Tham Số
```PowerShell
# myscriptparam.ps1
param (
    [string]$Name,
    [int]$Age
)
Write-Host "Xin chào, $Name! Bạn $Age tuổi."
```
Chạy với `.\myscriptparam.ps1 -Name Sarah -Age 32`, xuất ra:
```
Xin chào, Sarah! Bạn 32 tuổi.
```

## Sâu xa hơn

Cách tiếp cận hiện đại của PowerShell đối với các đối số dòng lệnh giống như một di sản từ các tiền nhiệm như cmd và Bash. Tuy nhiên, nó tăng cường sự linh hoạt và độ chính xác.

### Bối cảnh Lịch sử
Nhiều năm về trước, các tệp batch và kịch bản shell truy cập các đối số bằng các biến số (như `%1`, `%2`). PowerShell đã tinh chỉnh điều này với `$args` và các tham số đặt tên cho sự rõ ràng và kiểm soát tốt hơn.

### Các Phương Án Khác
Có các phương án khác, như phân tích đầu vào thô với `Read-Host` hoặc chấp nhận đầu vào từ pipeline. Tuy nhiên, `$args` và các tham số dễ dàng hơn cho các tác vụ tự động và kịch bản.

### Chi Tiết Triển Khai
`$args` là một mảng đơn giản, tốt cho đầu vào tùy ý. Các tham số, với các thuộc tính và loại của chúng, có thể xác thực đầu vào và thậm chí nhắc người dùng, làm cho kịch bản tự giải thích và ít nghiêng về lỗi hơn.

## Xem Thêm

- [Về Tham Số](https://docs.microsoft.com/en-us/powershell/scripting/developer/cmdlet/cmdlet-parameter-sets?view=powershell-7)
- [Biến Tự Động trong PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7&viewFallbackFrom=powershell-6)

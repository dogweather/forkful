---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:44.888110-07:00
description: "L\xE0m th\u1EBF n\xE0o PowerShell \u0111\u1ECDc c\xE1c \u0111\u1ED1\
  i s\u1ED1 d\xF2ng l\u1EC7nh b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng m\u1EA3ng `$args`\
  \ ho\u1EB7c c\xE1c tham s\u1ED1. `$args` nhanh cho c\xE1c k\u1ECBch b\u1EA3n s\u1EED\
  \ d\u1EE5ng m\u1ED9t l\u1EA7n; c\xE1c tham\u2026"
lastmod: '2024-04-05T21:53:38.319527-06:00'
model: gpt-4-0125-preview
summary: "`$args` nhanh cho c\xE1c k\u1ECBch b\u1EA3n s\u1EED d\u1EE5ng m\u1ED9t l\u1EA7\
  n; c\xE1c tham s\u1ED1 t\u1ED1t h\u01A1n cho c\xE1c c\xF4ng c\u1EE5 m\u1EA1nh m\u1EBD\
  ."
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:15.592065-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y vi\u1EBFt m\u1ED9t h\xE0m \u0111\u1EC3\
  \ t\xEDnh t\u1ED5ng c\u1EE7a hai s\u1ED1. \u0110\u01A1n gi\u1EA3n, nh\u01B0ng n\xF3\
  \ minh h\u1ECDa \u0111\u01B0\u1EE3c \xFD."
lastmod: '2024-03-13T22:44:36.943784-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y vi\u1EBFt m\u1ED9t h\xE0m \u0111\u1EC3 t\xEDnh t\u1ED5ng c\u1EE7\
  a hai s\u1ED1."
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
weight: 18
---

## Làm thế nào:
Hãy viết một hàm để tính tổng của hai số. Đơn giản, nhưng nó minh họa được ý.

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# Gọi hàm với 5 và 10
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "Tổng là $sum"
```

Mẫu đầu ra:

```
Tổng là 15
```

## Sâu hơn nữa
Hàm trong PowerShell, như trong hầu hết các ngôn ngữ, không phải là tin mới. Chúng ta đã được phân vùng mã từ những ngày của Fortran. Nó về việc 'không tái sáng tạo bánh xe'. Các phương án khác? Chắc chắn, kịch bản hoặc cmdlets. Nhưng chúng thiếu sự gọn gàng và nhạy cảm với ngữ cảnh của các hàm trong kịch bản.

Thực hiện? Hàm có thể đơn giản như ví dụ của chúng tôi hoặc phức tạp với phạm vi, đầu vào ống dẫn, và hơn thế nữa. Lấy `Advanced Functions`. Chúng mô phỏng cmdlets với các tham số có thuộc tính, như `[Parameter(Mandatory=$true)]`. Đó là một chút về sự linh hoạt của PowerShell.

## Xem thêm
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)

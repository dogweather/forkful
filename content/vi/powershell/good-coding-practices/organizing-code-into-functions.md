---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:15.592065-07:00
description: "T\u1ED5 ch\u1EE9c m\xE3 th\xE0nh c\xE1c h\xE0m l\xE0 vi\u1EC7c \u0111\
  \xF3ng g\xF3i c\xE1c kh\u1ED1i m\xE3 th\u1EF1c hi\u1EC7n c\xE1c nhi\u1EC7m v\u1EE5\
  \ c\u1EE5 th\u1EC3 v\xE0 \u0111\u1EB7t cho ch\xFAng m\u1ED9t t\xEAn. Vi\u1EC7c n\xE0\
  y \u0111\u01B0\u1EE3c th\u1EF1c hi\u1EC7n \u0111\u1EC3 l\xE0m cho m\xE3 c\xF3 th\u1EC3\
  \u2026"
lastmod: '2024-03-13T22:44:36.943784-06:00'
model: gpt-4-0125-preview
summary: "T\u1ED5 ch\u1EE9c m\xE3 th\xE0nh c\xE1c h\xE0m l\xE0 vi\u1EC7c \u0111\xF3\
  ng g\xF3i c\xE1c kh\u1ED1i m\xE3 th\u1EF1c hi\u1EC7n c\xE1c nhi\u1EC7m v\u1EE5 c\u1EE5\
  \ th\u1EC3 v\xE0 \u0111\u1EB7t cho ch\xFAng m\u1ED9t t\xEAn."
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

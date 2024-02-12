---
title:                "Sắp xếp mã thành các hàm"
aliases:
- /vi/powershell/organizing-code-into-functions/
date:                  2024-01-28T22:03:15.592065-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Tổ chức mã thành các hàm là việc đóng gói các khối mã thực hiện các nhiệm vụ cụ thể và đặt cho chúng một tên. Việc này được thực hiện để làm cho mã có thể tái sử dụng, dễ đọc và dễ bảo trì. Thay vì viết lại cùng một mã, hãy gọi một hàm. Muốn khắc phục sự cố hoặc nâng cấp? Chỉ cần điều chỉnh hàm mà không cần lục lọi qua đống kịch bản.

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

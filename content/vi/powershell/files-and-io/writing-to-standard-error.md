---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:48.660130-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: M\u1EABu \u0111\u1EA7u ra."
lastmod: '2024-04-05T21:53:38.320498-06:00'
model: gpt-4-0125-preview
summary: ''
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

## Cách thực hiện:
```PowerShell
# Viết một lỗi đơn giản vào stderr
Write-Host "Ôi, đã xảy ra lỗi!" -ForegroundColor Đỏ 1>&2

# Viết lỗi sử dụng cmdlet Write-Error
Write-Error "Đây là thông điệp lỗi!"

# Sử dụng $ErrorView để hiển thị hoặc xử lý lỗi một cách khác
$ErrorView = "CategoryView"
try {
    Get-ChildItem "nonexistentfile.txt"
} catch {
    Write-Host $_.Exception.Message -ForegroundColor Đỏ 1>&2
}
```

Mẫu đầu ra:
```
Ôi, đã xảy ra lỗi!
Write-Error: Đây là thông điệp lỗi!
Get-ChildItem: Không thể tìm thấy đường dẫn 'C:\...\nonexistentfile.txt' vì nó không tồn tại.
```

## Sâu hơn
Theo truyền thống, việc tách biệt stdout và stderr có gốc Unix, cho phép người dùng điều hướng đầu ra riêng biệt. PowerShell, kế thừa khái niệm này, sử dụng Write-Error và Write-Host (với một lệnh điều hướng), cùng với các cmdlet khác, để gửi thông điệp đến stderr. Ở bên dưới, PowerShell bọc các phương pháp .NET để thực hiện tính năng này.

Các phương pháp thay thế bao gồm sử dụng các câu lệnh throw hoặc các khối xử lý ngoại lệ; tuy nhiên, những điều này ảnh hưởng đến dòng chảy của kịch bản. Viết vào stderr không làm gián đoạn thực hiện trừ khi bạn cụ thể kiểm tra biến $Error hoặc sử dụng các tham số -ErrorAction.

## Xem thêm
- [about_Redirection](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_redirection)
- [Write-Error](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/write-error)
- [about_Try_Catch_Finally](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-exceptions?view=powershell-7.1)

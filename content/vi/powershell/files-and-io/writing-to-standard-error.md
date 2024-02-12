---
title:                "Ghi vào lỗi chuẩn"
aliases:
- /vi/powershell/writing-to-standard-error/
date:                  2024-01-28T22:13:48.660130-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Viết vào lỗi chuẩn (stderr) gửi thông điệp lỗi và chẩn đoán riêng biệt từ đầu ra chuẩn (stdout). Lập trình viên làm điều này để tách biệt rõ ràng đầu ra chương trình thông thường khỏi thông tin lỗi, làm cho việc gỡ lỗi và ghi nhật ký trở nên dễ dàng hơn.

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

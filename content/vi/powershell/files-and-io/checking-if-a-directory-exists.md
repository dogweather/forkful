---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:37.121578-07:00
description: "Vi\u1EC7c ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3\
  n t\u1EA1i kh\xF4ng ch\xEDnh l\xE0 x\xE1c \u0111\u1ECBnh xem m\u1ED9t th\u01B0 m\u1EE5\
  c c\xF3 \u1EDF m\u1ED9t v\u1ECB tr\xED ch\u1EC9 \u0111\u1ECBnh trong h\u1EC7 th\u1ED1\
  ng t\u1EC7p hay kh\xF4ng. C\xE1c l\u1EADp tr\xECnh vi\xEAn\u2026"
lastmod: '2024-02-25T18:49:35.301692-07:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1\
  i kh\xF4ng ch\xEDnh l\xE0 x\xE1c \u0111\u1ECBnh xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3\
  \ \u1EDF m\u1ED9t v\u1ECB tr\xED ch\u1EC9 \u0111\u1ECBnh trong h\u1EC7 th\u1ED1\
  ng t\u1EC7p hay kh\xF4ng. C\xE1c l\u1EADp tr\xECnh vi\xEAn\u2026"
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Việc kiểm tra xem một thư mục có tồn tại không chính là xác định xem một thư mục có ở một vị trí chỉ định trong hệ thống tệp hay không. Các lập trình viên làm điều này để ngăn chặn lỗi, quản lý tệp tin một cách hiệu quả, và đảm bảo dữ liệu được ghi hoặc đọc từ những vị trí chính xác.

## Làm thế nào:

Sử dụng cmdlet `Test-Path` để kiểm tra sự tồn tại của một thư mục. Cmdlet này trả về một boolean: `$true` nếu thư mục tồn tại, và `$false` nếu nó không tồn tại.

```PowerShell
# Kiểm tra xem một thư mục có tồn tại không
$directoryPath = "C:\ExampleFolder"
$exists = Test-Path $directoryPath
Write-Output $exists  # In ra True hoặc False
```

Kết quả mẫu:
```
True
```
hoặc nếu thư mục không tồn tại:
```
False
```

Bạn cũng có thể sử dụng nó trực tiếp trong một câu lệnh `if`:

```PowerShell
# Sử dụng Test-Path trong một câu lệnh if
if (Test-Path $directoryPath) {
    Write-Output "Yep, nó ở đó."
} else {
    Write-Output "Nope, không tìm thấy."
}
```

## Tìm hiểu sâu

Cmdlet `Test-Path` đã có từ PowerShell v1.0. Nó không chỉ là một 'chú ngựa một chiêu'; bên cạnh việc kiểm tra thư mục, nó cũng có thể được sử dụng để kiểm tra tệp tin, khóa đăng ký, và các mục khác thông qua các 'đường dẫn' khác nhau.

Có những phương án thay thế. PowerShell được xây dựng dựa trên .NET Framework, vì vậy bạn có thể chuyển xuống sử dụng các phương pháp .NET nếu muốn:

```PowerShell
[system.io.directory]::Exists($directoryPath)
```

Phương pháp này phục vụ cùng một mục đích nhưng tiếp cận theo cách "vòng vo". Tại sao phải bận tâm, khi `Test-Path` đã được tạo ra cho công việc này?

Về mặt triển khai, việc kiểm tra một thư mục trước khi thực hiện các hoạt động là một phương pháp hay nhất. Nói về việc dự đoán được. Bạn sẽ không đua xe với bình xăng trống, phải không? Vậy bạn cũng không nên đọc từ hoặc ghi vào một thư mục không tồn tại.

## Xem thêm

Để biết thêm thông tin, hãy tìm hiểu qua các liên kết này:

- [Tài liệu Cmdlet Test-Path](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path)
- [Phương pháp .NET Directory.Exists](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)

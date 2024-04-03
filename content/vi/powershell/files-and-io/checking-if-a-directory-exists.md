---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:37.121578-07:00
description: "L\xE0m th\u1EBF n\xE0o: S\u1EED d\u1EE5ng cmdlet `Test-Path` \u0111\u1EC3\
  \ ki\u1EC3m tra s\u1EF1 t\u1ED3n t\u1EA1i c\u1EE7a m\u1ED9t th\u01B0 m\u1EE5c. Cmdlet\
  \ n\xE0y tr\u1EA3 v\u1EC1 m\u1ED9t boolean: `$true` n\u1EBFu th\u01B0 m\u1EE5c t\u1ED3\
  n t\u1EA1i, v\xE0 `$false`\u2026"
lastmod: '2024-03-13T22:44:36.955213-06:00'
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng cmdlet `Test-Path` \u0111\u1EC3 ki\u1EC3m tra s\u1EF1\
  \ t\u1ED3n t\u1EA1i c\u1EE7a m\u1ED9t th\u01B0 m\u1EE5c."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

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

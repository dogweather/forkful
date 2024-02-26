---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:32.081683-07:00
description: "YAML l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1ng serialization d\u1EEF li\u1EC7\
  u th\xE2n thi\u1EC7n v\u1EDBi con ng\u01B0\u1EDDi. C\xE1c l\u1EADp tr\xECnh vi\xEA\
  n s\u1EED d\u1EE5ng n\xF3 cho c\xE1c t\u1EC7p c\u1EA5u h\xECnh, trao \u0111\u1ED5\
  i d\u1EEF li\u1EC7u gi\u1EEFa c\xE1c ng\xF4n\u2026"
lastmod: '2024-02-25T18:49:35.309483-07:00'
model: gpt-4-0125-preview
summary: "YAML l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1ng serialization d\u1EEF li\u1EC7\
  u th\xE2n thi\u1EC7n v\u1EDBi con ng\u01B0\u1EDDi. C\xE1c l\u1EADp tr\xECnh vi\xEA\
  n s\u1EED d\u1EE5ng n\xF3 cho c\xE1c t\u1EC7p c\u1EA5u h\xECnh, trao \u0111\u1ED5\
  i d\u1EEF li\u1EC7u gi\u1EEFa c\xE1c ng\xF4n\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
YAML là một định dạng serialization dữ liệu thân thiện với con người. Các lập trình viên sử dụng nó cho các tệp cấu hình, trao đổi dữ liệu giữa các ngôn ngữ, và bởi vì nó dễ đọc và viết so với XML hoặc JSON.

## Làm thế nào:
Để làm việc với YAML trong PowerShell, bạn sẽ cần sử dụng một module như `powershell-yaml`. Cài đặt nó trước tiên:

```PowerShell
Install-Module -Name powershell-yaml
```

Đọc nội dung YAML:

```PowerShell
# Nhập module
Import-Module powershell-yaml

# Tải một tệp YAML
$yamlContent = Get-Content -Path 'config.yaml' -Raw

# Chuyển đổi YAML thành một đối tượng PowerShell
$configObject = ConvertFrom-Yaml -Yaml $yamlContent

# Xuất ra đối tượng
$configObject
```

Tạo và viết YAML:

```PowerShell
# Tạo một bảng băm
$person = @{
  name = 'Jane Doe'
  age = 30
  languages = @('English', 'French')
}

# Chuyển đổi bảng băm thành YAML
$yamlOutput = ConvertTo-Yaml -Data $person

# Ghi YAML vào một tệp
$yamlOutput | Out-File -FilePath 'person.yaml'
```

## Sâu hơn nữa
YAML xuất xứ từ đầu những năm 2000 và có nghĩa là “YAML Ain't Markup Language,” một từ viết tắt đệ quy nhấn mạnh cách tiếp cận dựa trên dữ liệu hơn so với các ngôn ngữ đánh dấu như HTML. Mặc dù JSON thường là lựa chọn hàng đầu cho API và dịch vụ web do khả năng phân tích cú pháp hiệu quả và tính gọn nhẹ, YAML vẫn phổ biến vì tính dễ đọc và dễ chỉnh sửa hơn, đặc biệt trong các tệp cấu hình (ví dụ, Docker Compose và Kubernetes).

Các lựa chọn thay thế cho `powershell-yaml` bao gồm `YamlDotNet` với mã dính `.NET`, hoặc phân tích cú pháp chuỗi YAML một cách thủ công - nhưng tại sao làm phức tạp cuộc sống của bạn?

Bên dưới, `powershell-yaml` sử dụng `YamlDotNet`, chuyển đổi YAML thành các đối tượng .NET mà PowerShell có thể dễ dàng xử lý. Sự kết hợp này cho phép chuyển đổi dữ liệu YAML một cách mượt mà vào hệ sinh thái PowerShell.

## Xem thêm
- [`powershell-yaml` trên PowerShell Gallery](https://www.powershellgallery.com/packages/powershell-yaml)
- [Trang Web Chính Thức của YAML](https://yaml.org/)
- [Tham Khảo Cú Pháp YAML](https://learnxinyminutes.com/docs/yaml/)

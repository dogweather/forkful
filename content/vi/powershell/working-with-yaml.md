---
title:                "Làm việc với YAML"
date:                  2024-01-28T22:12:32.081683-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

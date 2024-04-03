---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:44.033420-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong PowerShell, kh\xF4ng c\xF3 cmdlet d\xE0\
  nh ri\xEAng \u0111\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p TOML. B\u1EA1n th\u01B0\u1EDD\
  ng s\u1EED d\u1EE5ng m\u1ED9t module ho\u1EB7c chuy\u1EC3n \u0111\u1ED5i TOML sang\
  \ JSON b\u1EB1ng\u2026"
lastmod: '2024-03-13T22:44:36.965946-06:00'
model: gpt-4-0125-preview
summary: "Trong PowerShell, kh\xF4ng c\xF3 cmdlet d\xE0nh ri\xEAng \u0111\u1EC3 ph\xE2\
  n t\xEDch c\xFA ph\xE1p TOML."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

## Làm thế nào:
Trong PowerShell, không có cmdlet dành riêng để phân tích cú pháp TOML. Bạn thường sử dụng một module hoặc chuyển đổi TOML sang JSON bằng công cụ như `toml-to-json` nếu bạn muốn làm việc với PowerShell. Dưới đây là cách bạn thực hiện với một module giả định `PowerShellTOML`:

```PowerShell
# Đầu tiên, cài đặt module (ảo, cho mục đích minh họa)
Install-Module PowerShellTOML

# Nhập một tệp TOML
$config = Import-TomlConfig -Path './config.toml'

# Truy cập một giá trị
Write-Output $config.database.server

# Nội dung TOML mẫu trong 'config.toml':
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# Đầu ra mẫu:
# 192.168.1.1
```

## Sâu hơn
TOML được tạo ra bởi Tom Preston-Werner, đồng sáng lập của GitHub, như một lựa chọn đơn giản hơn so với XML và YAML cho các tệp cấu hình. Phiên bản đầu tiên xuất hiện vào năm 2013. TOML có thể so sánh với JSON nhưng được thiết kế để thân thiện hơn với con người, làm cho nó trở thành một lựa chọn tốt cho cấu hình được duy trì bởi con người. Các lựa chọn thay thế bao gồm YAML, JSON và XML.

Về mặt triển khai, một module PowerShell cho TOML thường là một bộ bao quanh một thư viện TOML viết bằng một ngôn ngữ hiệu suất cao hơn như C#. PowerShell không có hỗ trợ tích hợp sẵn cho TOML, đó là lý do tại sao một module như vậy là cần thiết để giao tiếp với định dạng TOML một cách tiện lợi.

## Xem thêm
- Tiêu chuẩn TOML: https://toml.io/en/
- Kho lưu trữ GitHub cho module `toml` PowerShell (nếu tồn tại tại thời điểm đọc): https://github.com/powershell/PowerShellTOML
- Giới thiệu về TOML: https://github.com/toml-lang/toml
- So sánh các định dạng tạo chuỗi dữ liệu: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats

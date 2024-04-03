---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:54.036773-07:00
description: "X\xF3a c\xE1c k\xFD t\u1EF1 kh\u1EDBp v\u1EDBi m\u1ED9t m\u1EABu nh\u1EA5\
  t \u0111\u1ECBnh ngh\u0129a l\xE0 lo\u1EA1i b\u1ECF nh\u1EEFng ph\u1EA7n kh\xF4\
  ng mong mu\u1ED1n kh\u1ECFi chu\u1ED7i c\u1EE7a b\u1EA1n - h\xE3y ngh\u0129 \u0111\
  \u1EBFn vi\u1EC7c l\xE0m s\u1EA1ch d\u1EEF li\u1EC7u ho\u1EB7c ph\xE2n\u2026"
lastmod: '2024-03-13T22:44:36.911803-06:00'
model: gpt-4-0125-preview
summary: "X\xF3a c\xE1c k\xFD t\u1EF1 kh\u1EDBp v\u1EDBi m\u1ED9t m\u1EABu nh\u1EA5\
  t \u0111\u1ECBnh ngh\u0129a l\xE0 lo\u1EA1i b\u1ECF nh\u1EEFng ph\u1EA7n kh\xF4\
  ng mong mu\u1ED1n kh\u1ECFi chu\u1ED7i c\u1EE7a b\u1EA1n - h\xE3y ngh\u0129 \u0111\
  \u1EBFn vi\u1EC7c l\xE0m s\u1EA1ch d\u1EEF li\u1EC7u ho\u1EB7c ph\xE2n t\xEDch c\xE1\
  c t\u1EC7p v\u0103n b\u1EA3n."
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
weight: 5
---

## Làm thế nào:
PowerShell sử dụng toán tử `-replace` để xóa các ký tự khớp với một mẫu. Dưới đây là một số hành động sửa chuỗi cho bạn:

```PowerShell
# Thay thế đơn giản: loại bỏ chữ số
$text = 'ABC123'
$cleanText = $text -replace '\d+'
$cleanText  # Đầu ra: ABC

# Loại bỏ khoảng trắng
$text = 'Hello World         '
$trimmedText = $text -replace '\s+$'
$trimmedText  # Đầu ra: Hello World

# Loại bỏ các ký tự cụ thể
$text = 'uN_w@nt3d-charact3r$'
$cleanedUpText = $text -replace '[-@3$]', ''
$cleanedUpText  # Đầu ra: uNwntd-charactr
```

## Sâu hơn nữa
Toán tử `-replace` của PowerShell là một công cụ mạnh mẽ mà sử dụng regex (biểu thức chính quy). Regex là một nghệ thuật gần như huyền bí; nó đã tồn tại từ những năm 1950 và hoạt động trên nhiều ngôn ngữ lập trình để khớp mẫu.

Có phương pháp thay thế cho `-replace` không? Đối với những việc đơn giản, có họ phương pháp `.Trim()` dành cho khoảng trắng và phương thức `.Replace()` cho các thay thế theo nghĩa đen. Nhưng toán tử `-replace` là lựa chọn của bạn cho các thao tác dựa trên mẫu.

Bên dưới capo, khi bạn sử dụng `-replace`, PowerShell tận dụng khả năng regex của .NET Framework. Đó là một hoạt động khớp và cắt mạnh mẽ hoạt động ở cấp độ từng ký tự để quyết định cái nào ở lại và cái nào đi. Hãy nhớ, các mẫu regex có thể trở nên phức tạp và tiêu thụ nhiều hơn năng lực xử lý cho các mẫu phức tạp, vì vậy sử dụng cẩn thận!

## Xem thêm
Để đi sâu hơn vào hố thỏ regex, hãy xem những cái này:
- [Về Toán tử So sánh của PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1)
- [Tự động hóa Những Việc Nhàm Chán với PowerShell](https://adamtheautomator.com/powershell-replace/) cho ứng dụng trong thế giới thực.

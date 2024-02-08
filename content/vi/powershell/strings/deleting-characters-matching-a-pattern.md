---
title:                "Xóa các ký tự phù hợp với một mẫu"
date:                  2024-01-28T21:58:54.036773-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Xóa các ký tự khớp với một mẫu nhất định nghĩa là loại bỏ những phần không mong muốn khỏi chuỗi của bạn - hãy nghĩ đến việc làm sạch dữ liệu hoặc phân tích các tệp văn bản. Các lập trình viên làm điều này để trích xuất thông tin có ý nghĩa, đảm bảo tính nhất quán của dữ liệu, hoặc chuẩn bị dữ liệu cho quá trình xử lý.

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

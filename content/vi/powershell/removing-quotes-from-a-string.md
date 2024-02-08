---
title:                "Loại bỏ dấu ngoặc kép khỏi chuỗi"
date:                  2024-01-28T22:06:21.315141-07:00
model:                 gpt-4-0125-preview
simple_title:         "Loại bỏ dấu ngoặc kép khỏi chuỗi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc loại bỏ dấu nháy khỏi một chuỗi trong PowerShell giúp loại bỏ dấu nháy đơn (`'`) hoặc dấu nháy kép (`"`) bao quanh văn bản của bạn. Lập trình viên thường cần phải làm sạch chuỗi cho việc xử lý, so sánh, hoặc mục đích xuất ra, đặc biệt khi xử lí đầu vào từ người dùng hoặc phân tích tệp.

## Làm thế nào:
Bạn có thể sử dụng toán tử `-replace` để loại bỏ dấu nháy khỏi chuỗi. Dưới đây là cách làm:

```PowerShell
# Thay thế dấu nháy đơn
$stringWithSingleQuotes = "'Xin chào, Thế giới!'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # Kết quả: Xin chào, Thế giới!

# Thay thế dấu nháy kép
$stringWithDoubleQuotes = '"Xin chào, Thế giới!"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # Kết quả: Xin chào, Thế giới!
```

Cho cả hai loại:

```PowerShell
$stringWithQuotes = '"Chào bạn," cô ấy nói.'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # Chú ý đến việc sử dụng lớp ký tự regex
Write-Output $cleanString  # Kết quả: Chào bạn, cô ấy nói.
```

Kết quả mẫu từ bảng điều khiển sẽ trông như thế này:

```
Xin chào, Thế giới!
Xin chào, Thế giới!
Chào bạn, cô ấy nói.
```

## Sâu hơn
Trước kia, trước khi PowerShell trở thành một phần của Microsoft, xử lý văn bản trong Windows thường là lĩnh vực của các kịch bản batch có khả năng hạn chế. Sự giới thiệu của PowerShell mang lại tính năng mạnh mẽ trong việc thao tác chuỗi, khiến việc lập kịch bản trở nên mạnh mẽ hơn nhiều.

Có những phương pháp khác ngoài `-replace`, chẳng hạn như sử dụng phương pháp `.Trim()` để loại bỏ dấu nháy chỉ ở đầu và cuối chuỗi, nhưng chúng không cung cấp cùng một kiểm soát hoặc hỗ trợ regex.

```PowerShell
# Sử dụng .Trim() cho dấu nháy ở đầu và cuối
$stringWithQuotes = '"Xin chào, Thế giới!"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # Kết quả: Xin chào, Thế giới!
```

Lưu ý, `-replace` sử dụng regex đằng sau hậu trường, vì vậy khi bạn làm việc với nó, nhớ rằng các ký tự đặc biệt cần được thoát nếu bạn đang nhắm vào chúng. Nếu bạn cần kiểm soát chi tiết hơn đối với việc loại bỏ dấu nháy, việc đào sâu vào regex với `-replace` là con đường đi, mang lại cho bạn sự linh hoạt to lớn.

## Xem thêm
- Để biết thêm về regex trong PowerShell, xem tài liệu chính thức: [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- Khám phá các phương pháp xử lý chuỗi khác: [Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/en-us/dotnet/api/system.string.trim?view=net-6.0)

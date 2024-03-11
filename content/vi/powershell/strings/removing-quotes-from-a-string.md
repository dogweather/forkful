---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:21.315141-07:00
description: "Vi\u1EC7c lo\u1EA1i b\u1ECF d\u1EA5u nh\xE1y kh\u1ECFi m\u1ED9t chu\u1ED7\
  i trong PowerShell gi\xFAp lo\u1EA1i b\u1ECF d\u1EA5u nh\xE1y \u0111\u01A1n (`'`)\
  \ ho\u1EB7c d\u1EA5u nh\xE1y k\xE9p (`\"`) bao quanh v\u0103n b\u1EA3n c\u1EE7a\
  \ b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn\u2026"
lastmod: '2024-03-11T00:14:10.210199-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c lo\u1EA1i b\u1ECF d\u1EA5u nh\xE1y kh\u1ECFi m\u1ED9t chu\u1ED7\
  i trong PowerShell gi\xFAp lo\u1EA1i b\u1ECF d\u1EA5u nh\xE1y \u0111\u01A1n (`'`)\
  \ ho\u1EB7c d\u1EA5u nh\xE1y k\xE9p (`\"`) bao quanh v\u0103n b\u1EA3n c\u1EE7a\
  \ b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn\u2026"
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
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

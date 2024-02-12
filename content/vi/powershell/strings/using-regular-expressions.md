---
title:                "Sử dụng biểu thức chính quy"
aliases: - /vi/powershell/using-regular-expressions.md
date:                  2024-01-28T22:09:46.166345-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Biểu thức chính quy (regex) là những mẫu mạnh mẽ dùng để tìm kiếm các chuỗi văn bản. Lập trình viên sử dụng regex để tìm kiếm, xác thực hoặc thay thế nội dung một cách hiệu quả.

## Làm thế nào:
```PowerShell
# Khớp mẫu bắt đầu bằng 'S' theo sau là bất kỳ ký tự nào, kết thúc bằng 'e'
$pattern = 'S.*e'
$text = 'Sample sentence in PowerShell.'
if ($text -match $pattern) {
    "Tìm thấy khớp: $($matches[0])"
}

# Thay thế tất cả các lần xuất hiện của 'dog' bằng 'cat'
$petStory = 'The quick brown dog jumps over the lazy dog.'
$petStory -replace 'dog', 'cat'
```
Kết quả:
```
Tìm thấy khớp: Sample sentence in
The quick brown cat jumps over the lazy cat.
```

## Sâu hơn
Regex đã trở thành một phần không thể thiếu trong lập trình kể từ những năm 1950. Trong khi PowerShell có các cmdlets tích hợp sẵn như `-match`, `-replace`, và `Select-String` cho regex, cũng có các phương pháp thay thế khác cho việc thao tác văn bản – có thể kể đến `string.Contains` hoặc `string.Replace`. Regex trong PowerShell sử dụng hiện thực từ khung .NET, do đó nó mạnh mẽ và đầy đủ tính năng.

## Xem thêm
- [Tài liệu tham khảo chính thức về regex của Microsoft](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions)
- [Regular-Expressions.info](https://www.regular-expressions.info/powershell.html)
- [Regex101: Xây dựng và kiểm thử regex](https://regex101.com/)

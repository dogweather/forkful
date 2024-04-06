---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:46.166345-07:00
description: "L\xE0m th\u1EBF n\xE0o: Regex \u0111\xE3 tr\u1EDF th\xE0nh m\u1ED9t\
  \ ph\u1EA7n kh\xF4ng th\u1EC3 thi\u1EBFu trong l\u1EADp tr\xECnh k\u1EC3 t\u1EEB\
  \ nh\u1EEFng n\u0103m 1950. Trong khi PowerShell c\xF3 c\xE1c cmdlets t\xEDch h\u1EE3\
  p s\u1EB5n nh\u01B0\u2026"
lastmod: '2024-04-05T22:50:51.234624-06:00'
model: gpt-4-0125-preview
summary: "Regex \u0111\xE3 tr\u1EDF th\xE0nh m\u1ED9t ph\u1EA7n kh\xF4ng th\u1EC3\
  \ thi\u1EBFu trong l\u1EADp tr\xECnh k\u1EC3 t\u1EEB nh\u1EEFng n\u0103m 1950."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

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

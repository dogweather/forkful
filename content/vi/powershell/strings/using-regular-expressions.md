---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:46.166345-07:00
description: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 nh\u1EEFng m\u1EABu m\u1EA1\
  nh m\u1EBD d\xF9ng \u0111\u1EC3 t\xECm ki\u1EBFm c\xE1c chu\u1ED7i v\u0103n b\u1EA3\
  n. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng regex \u0111\u1EC3 t\xECm ki\u1EBF\
  m, x\xE1c th\u1EF1c ho\u1EB7c thay th\u1EBF n\u1ED9i\u2026"
lastmod: '2024-02-25T18:49:35.265271-07:00'
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 nh\u1EEFng m\u1EABu m\u1EA1\
  nh m\u1EBD d\xF9ng \u0111\u1EC3 t\xECm ki\u1EBFm c\xE1c chu\u1ED7i v\u0103n b\u1EA3\
  n. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng regex \u0111\u1EC3 t\xECm ki\u1EBF\
  m, x\xE1c th\u1EF1c ho\u1EB7c thay th\u1EBF n\u1ED9i\u2026"
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
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

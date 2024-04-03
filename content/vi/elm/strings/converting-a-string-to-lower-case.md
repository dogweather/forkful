---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:01.338009-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Elm s\u1EED d\u1EE5ng h\xE0m `String.toLower`\
  \ \u0111\u1EC3 chuy\u1EC3n \u0111\u1ED5i v\u0103n b\u1EA3n."
lastmod: '2024-03-13T22:44:36.526918-06:00'
model: gpt-4-0125-preview
summary: "Elm s\u1EED d\u1EE5ng h\xE0m `String.toLower` \u0111\u1EC3 chuy\u1EC3n \u0111\
  \u1ED5i v\u0103n b\u1EA3n."
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

## Cách thực hiện:
Elm sử dụng hàm `String.toLower` để chuyển đổi văn bản:

```elm
import String

lowercaseString : String -> String
lowercaseString text =
    String.toLower text

-- Cách sử dụng
result : String
result =
    lowercaseString "HeLLo, WoRLD!"

-- Kết quả: "hello, world!"
```

## Sâu hơn
`String.toLower` của Elm xuất phát từ thư viện `String` cốt lõi của Elm, với việc xem xét đến quốc tế hóa. Lịch sử hóa, việc chuyển đổi chữ hoa sang chữ thường đã phát triển từ ASCII cơ bản đến hỗ trợ Unicode đầy đủ do nhu cầu xử lý văn bản quốc tế.

Trong một số ngôn ngữ như Javascript, có các phương án alternative như `toLowerCase()` và `toLocaleLowerCase()`, trong đó phần sau xem xét đến các quy tắc cụ thể của địa phương. Trong Elm, `String.toLower` sẽ đáp ứng đủ cho hầu hết trường hợp trừ khi xử lý các thao tác nhạy cảm với địa phương, có thể yêu cầu triển khai tùy chỉnh.

Một chi tiết cần nhớ là việc chuyển đổi chữ hoa sang chữ thường không luôn luôn là một chuyển đổi một chữ cái sang một chữ cái; một số ký tự có thể không có dạng chữ thường tương đương, và một số khác có thể thay đổi kích thước (ví dụ, chuyển đổi "ß" trong tiếng Đức).

## Xem Thêm
- Tài liệu Elm String: [https://package.elm-lang.org/packages/elm/core/latest/String#toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- Unicode Case Folding: [https://www.w3.org/International/wiki/Case_folding](https://www.w3.org/International/wiki/Case_folding)
- Vấn đề chuyển đổi chữ hoa, chữ thường cụ thể theo ngôn ngữ: [https://stackoverflow.com/questions/234591/upper-vs-lower-case](https://stackoverflow.com/questions/234591/upper-vs-lower-case)

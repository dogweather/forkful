---
title:                "Chuyển đổi chuỗi thành chữ thường"
date:                  2024-01-28T21:58:01.338009-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?

Chuyển đổi một chuỗi sang ký tự thường có nghĩa là biến đổi tất cả các ký tự chữ cái sang dạng chữ thường của chúng. Các lập trình viên thường làm điều này để so sánh không phân biệt chữ hoa chữ thường hoặc chuẩn hóa dữ liệu văn bản cho việc lưu trữ và xử lý.

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

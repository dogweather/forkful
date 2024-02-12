---
title:                "Xóa các ký tự phù hợp với một mẫu"
aliases: - /vi/elm/deleting-characters-matching-a-pattern.md
date:                  2024-01-28T21:58:35.468155-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Xóa các ký tự khớp với một mẫu có nghĩa là gỡ bỏ các tập hợp ký tự cụ thể khỏi văn bản, dựa trên các quy tắc (mẫu). Các lập trình viên làm điều này để làm sạch văn bản, xử lý dữ liệu, hoặc để đơn giản hóa dữ liệu nhập trước khi phân tích cú pháp.

## Làm thế nào:
Elm không hỗ trợ regex một cách tự nhiên, nhưng bạn có thể mô phỏng việc xóa ký tự. Dưới đây là ví dụ sử dụng `String.filter` để loại bỏ các chữ số khỏi một chuỗi.

```Elm
import Browser
import Html exposing (text)

removeDigits : String -> String
removeDigits = String.filter (\char -> not (char >= '0' && char <= '9'))

main =
  text (removeDigits "Elm 0.19.1 is super 123 cool!")

-- Đầu ra: "Elm . is super  cool!"
```

## Sâu hơn
Elm thiếu hỗ trợ regex là một phần của ngôn ngữ cốt lõi của mình, khác biệt so với nhiều ngôn ngữ khác. Sự lựa chọn thiết kế này phù hợp với mục tiêu về sự đơn giản và an toàn của Elm. Regex có thể gây ra lỗi và khó gỡ lỗi, nhưng Elm ủng hộ các thao tác chuỗi đơn giản hơn che lấp nhiều trường hợp sử dụng phổ biến.

Đối với các trường hợp cần đến regex, việc triển khai phụ thuộc vào giao tiếp JavaScript qua cổng (ports). Tuy nhiên, Elm khuyến khích tìm giải pháp trong ngôn ngữ trước tiên. Module `String` cung cấp các chức năng như `filter`, `replace`, và `split` che phủ một loạt các thao tác văn bản dựa trên mẫu mà không giới thiệu phức tạp của regex.

## Xem thêm
- [Tài liệu Elm String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Practical Elm for a Busy Developer](https://korban.net/elm/book/) - Cuốn sách bao gồm các tiện ích sửa đổi văn bản.

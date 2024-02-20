---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:35.468155-07:00
description: "X\xF3a c\xE1c k\xFD t\u1EF1 kh\u1EDBp v\u1EDBi m\u1ED9t m\u1EABu c\xF3\
  \ ngh\u0129a l\xE0 g\u1EE1 b\u1ECF c\xE1c t\u1EADp h\u1EE3p k\xFD t\u1EF1 c\u1EE5\
  \ th\u1EC3 kh\u1ECFi v\u0103n b\u1EA3n, d\u1EF1a tr\xEAn c\xE1c quy t\u1EAFc (m\u1EAB\
  u). C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\xE0\
  m\u2026"
lastmod: 2024-02-19 22:04:55.688646
model: gpt-4-0125-preview
summary: "X\xF3a c\xE1c k\xFD t\u1EF1 kh\u1EDBp v\u1EDBi m\u1ED9t m\u1EABu c\xF3 ngh\u0129\
  a l\xE0 g\u1EE1 b\u1ECF c\xE1c t\u1EADp h\u1EE3p k\xFD t\u1EF1 c\u1EE5 th\u1EC3\
  \ kh\u1ECFi v\u0103n b\u1EA3n, d\u1EF1a tr\xEAn c\xE1c quy t\u1EAFc (m\u1EABu).\
  \ C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\xE0\
  m\u2026"
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
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

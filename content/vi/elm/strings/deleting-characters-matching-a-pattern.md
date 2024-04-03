---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:35.468155-07:00
description: "L\xE0m th\u1EBF n\xE0o: Elm kh\xF4ng h\u1ED7 tr\u1EE3 regex m\u1ED9\
  t c\xE1ch t\u1EF1 nhi\xEAn, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 m\xF4 ph\u1ECFng\
  \ vi\u1EC7c x\xF3a k\xFD t\u1EF1. D\u01B0\u1EDBi \u0111\xE2y l\xE0 v\xED d\u1EE5\
  \ s\u1EED d\u1EE5ng `String.filter` \u0111\u1EC3 lo\u1EA1i b\u1ECF c\xE1c\u2026"
lastmod: '2024-03-13T22:44:36.523082-06:00'
model: gpt-4-0125-preview
summary: "Elm kh\xF4ng h\u1ED7 tr\u1EE3 regex m\u1ED9t c\xE1ch t\u1EF1 nhi\xEAn, nh\u01B0\
  ng b\u1EA1n c\xF3 th\u1EC3 m\xF4 ph\u1ECFng vi\u1EC7c x\xF3a k\xFD t\u1EF1."
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
weight: 5
---

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

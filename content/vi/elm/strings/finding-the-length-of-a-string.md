---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:18.514758-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Elm, b\u1EA1n s\u1EED d\u1EE5ng `String.length`\
  \ \u0111\u1EC3 bi\u1EBFt m\u1ED9t chu\u1ED7i ch\u1EE9a bao nhi\xEAu k\xFD t\u1EF1\
  . H\xE3y ch\u1EE9ng ki\u1EBFn."
lastmod: '2024-03-13T22:44:36.532046-06:00'
model: gpt-4-0125-preview
summary: "Trong Elm, b\u1EA1n s\u1EED d\u1EE5ng `String.length` \u0111\u1EC3 bi\u1EBF\
  t m\u1ED9t chu\u1ED7i ch\u1EE9a bao nhi\xEAu k\xFD t\u1EF1."
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

## Làm thế nào:
Trong Elm, bạn sử dụng `String.length` để biết một chuỗi chứa bao nhiêu ký tự. Hãy chứng kiến:

```elm
import Html exposing (text)

main =
  text (String.fromInt (String.length "Hello, Elm!"))
  -- Đầu ra: "11"
```

## Sâu hơn nữa
Trong lịch sử, các hàm đo chiều dài chuỗi đã rất quan trọng đối với quản lý bộ nhớ và xử lý văn bản trong các ngôn ngữ có quyền truy cập dữ liệu cấp thấp. Elm, với tính chất cấp cao, tóm tắt các chi tiết này, cung cấp chức năng đã được tích hợp sẵn với `String.length`.

Hai điểm đáng lưu ý:
1. Các chuỗi Elm được mã hóa UTF-16. `String.length` trả về số lượng đơn vị mã UTF-16, có thể khác với số lượng grapheme Unicode thực sự (các ký tự nhận thức bởi người dùng) trong các chuỗi với các ký tự phức tạp.
2. Không có phương án thay thế được tích hợp sẵn cho `String.length` trong Elm. Nếu bạn cần số lượng grapheme, bạn có thể cần một hàm tùy chỉnh tính toán những phức tạp của Unicode.

Ở bên trong, `String.length` lặp qua cấu trúc dữ liệu chuỗi, đếm các phần tử. Là một hàm thuần túy, kết quả đầu ra của nó chỉ phụ thuộc vào đầu vào, duy trì tinh thần lập trình hàm của Elm.

## Xem Thêm
- Tài liệu chính thức về `String` của Elm: [https://package.elm-lang.org/packages/elm/core/latest/String#length](https://package.elm-lang.org/packages/elm/core/latest/String#length)
- UTF-16: [https://en.wikipedia.org/wiki/UTF-16](https://en.wikipedia.org/wiki/UTF-16)

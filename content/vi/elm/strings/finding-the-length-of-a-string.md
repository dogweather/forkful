---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:18.514758-07:00
description: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0\
  \ \u0111\u1EBFm s\u1ED1 k\xFD t\u1EF1 c\u1EE7a n\xF3. L\u1EADp tr\xECnh vi\xEAn\
  \ th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 x\xE1c nh\u1EADn \u0111\u1EA7\
  u v\xE0o, thao t\xE1c v\u0103n b\u1EA3n, ho\u1EB7c \u0111\u01A1n gi\u1EA3n ch\u1EC9\
  \ \u0111\u1EC3\u2026"
lastmod: '2024-03-13T22:44:36.532046-06:00'
model: gpt-4-0125-preview
summary: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0 \u0111\
  \u1EBFm s\u1ED1 k\xFD t\u1EF1 c\u1EE7a n\xF3. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 x\xE1c nh\u1EADn \u0111\u1EA7u v\xE0o,\
  \ thao t\xE1c v\u0103n b\u1EA3n, ho\u1EB7c \u0111\u01A1n gi\u1EA3n ch\u1EC9 \u0111\
  \u1EC3\u2026"
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Gì & Tại Sao?
Tìm chiều dài của một chuỗi nghĩa là đếm số ký tự của nó. Lập trình viên thực hiện việc này để xác nhận đầu vào, thao tác văn bản, hoặc đơn giản chỉ để đánh giá kích thước dữ liệu.

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

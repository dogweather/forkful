---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:20.732929-07:00
description: "L\xE0m th\u1EBF n\xE0o: Elm c\xF3 m\u1ED9t to\xE1n t\u1EED tuy\u1EC7\
  t v\u1EDDi `(++)` \u0111\u1EC3 c\u1EE9u ng\xE0y."
lastmod: '2024-03-13T22:44:36.533281-06:00'
model: gpt-4-0125-preview
summary: "Elm c\xF3 m\u1ED9t to\xE1n t\u1EED tuy\u1EC7t v\u1EDDi `(++)` \u0111\u1EC3\
  \ c\u1EE9u ng\xE0y."
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

## Làm thế nào:
Elm có một toán tử tuyệt vời `(++)` để cứu ngày:

```Elm
greeting : String
greeting =
    "Hello, " ++ "world!"

-- "Hello, world!"
```

Nhưng đôi khi, bạn có một đống mảnh văn bản. Đừng lo, `++` có thể liên kết:

```Elm
fullName : String
fullName =
    "Elm" ++ " " ++ "Lang"

-- "Elm Lang"
```

Và đối với danh sách các chuỗi, `String.join` là người bạn của bạn:

```Elm
words : List String
words =
    ["Join", "the", "Elm", "club"]

sentence : String
sentence =
    String.join " " words

-- "Join the Elm club"
```

## Sâu hơn
Ngày xưa, bạn thường xuyên nối chuỗi với các hàm phức tạp trong các ngôn ngữ khác. Trong Elm, việc này luôn dễ dàng nhờ vào toán tử `(++)`. Nếu bạn thực sự nối rất nhiều, hiệu suất có thể trở thành một vấn đề; sử dụng `(++)` trên các chuỗi dài có thể chậm hơn, bởi vì Elm phải đi qua toàn bộ chuỗi ở bên trái của `(++)` mỗi lần.

Một số ngôn ngữ còn có "nội suy chuỗi", nhưng Elm không hỗ trợ nội suy chuỗi. Không sao cả, `(++)` và `String.join` đã giúp chúng ta.

Bên trong, khi Elm nối chuỗi, nó cố gắng thông minh về nó, thường sử dụng các hoạt động JavaScript được tối ưu, đó là cái mà Elm được biên dịch xuống cuối cùng. Vì vậy, ngay cả khi `(++)` có vẻ đơn giản, có một số sự tinh tế đang diễn ra phía sau hậu trường để giữ mọi thứ nhanh chóng.

## Xem thêm
- Tài liệu chính thức của Elm về Chuỗi: https://package.elm-lang.org/packages/elm/core/latest/String
- Hướng dẫn Elm, nơi bạn có thể tìm hiểu thêm về chuỗi: https://guide.elm-lang.org/strings/

---
aliases:
- /vi/elm/concatenating-strings/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:20.732929-07:00
description: "N\u1ED1i chu\u1ED7i c\xF3 ngh\u0129a l\xE0 gh\xE9p hai ho\u1EB7c nhi\u1EC1\
  u ph\u1EA7n v\u0103n b\u1EA3n l\u1EA1i v\u1EDBi nhau. \u0110\xF3 l\xE0 m\u1ED9t\
  \ vi\u1EC7c c\u01A1 b\u1EA3n v\xE0 thi\u1EBFt y\u1EBFu nh\u01B0 s\u1EED d\u1EE5\
  ng b\u0103ng d\xEDnh, cho ph\xE9p b\u1EA1n t\u1EA1o ra c\xE1c\u2026"
lastmod: 2024-02-18 23:08:50.594619
model: gpt-4-0125-preview
summary: "N\u1ED1i chu\u1ED7i c\xF3 ngh\u0129a l\xE0 gh\xE9p hai ho\u1EB7c nhi\u1EC1\
  u ph\u1EA7n v\u0103n b\u1EA3n l\u1EA1i v\u1EDBi nhau. \u0110\xF3 l\xE0 m\u1ED9t\
  \ vi\u1EC7c c\u01A1 b\u1EA3n v\xE0 thi\u1EBFt y\u1EBFu nh\u01B0 s\u1EED d\u1EE5\
  ng b\u0103ng d\xEDnh, cho ph\xE9p b\u1EA1n t\u1EA1o ra c\xE1c\u2026"
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Là gì & Tại sao?
Nối chuỗi có nghĩa là ghép hai hoặc nhiều phần văn bản lại với nhau. Đó là một việc cơ bản và thiết yếu như sử dụng băng dính, cho phép bạn tạo ra các chuỗi mới một cách linh hoạt để hiển thị thông điệp, tạo mẫu, và nhiều hơn nữa.

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

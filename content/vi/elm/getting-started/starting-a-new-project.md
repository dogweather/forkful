---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:31.270733-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Elm, b\u1EAFt \u0111\u1EA7u m\u1ECDi th\u1EE9\
  \ v\u1EDBi l\u1EC7nh `elm init`. \u0110i\u1EC1u h\u01B0\u1EDBng \u0111\u1EBFn th\u01B0\
  \ m\u1EE5c d\u1EF1 \xE1n c\u1EE7a b\u1EA1n v\xE0 kh\u1EDFi \u0111\u1ED9ng terminal."
lastmod: '2024-03-13T22:44:36.545142-06:00'
model: gpt-4-0125-preview
summary: "Trong Elm, b\u1EAFt \u0111\u1EA7u m\u1ECDi th\u1EE9 v\u1EDBi l\u1EC7nh `elm\
  \ init`."
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
weight: 1
---

## Làm thế nào:
Trong Elm, bắt đầu mọi thứ với lệnh `elm init`. Điều hướng đến thư mục dự án của bạn và khởi động terminal:

```shell
mkdir my-elm-project
cd my-elm-project
elm init
```

Lệnh này tạo ra một file `elm.json` và thư mục `src`. Dưới đây là ví dụ đơn giản "Hello, World!" bằng Elm:

```Elm
module Main exposing (..)

import Html exposing (text)

main =
    text "Hello, World!"
```

Khi bạn chạy nó với `elm reactor` và truy cập vào `http://localhost:8000`, nó sẽ hiển thị "Hello, World!" trong trình duyệt của bạn.

## Sâu hơn nữa
Elm ra đời vào năm 2012, với mục tiêu làm cho việc phát triển front-end trở nên dễ dàng hơn. Đó không chỉ là về việc tránh lỗi thời gian chạy; Elm mang đến một trọng tâm mạnh mẽ vào sự đơn giản và hạnh phúc của nhà phát triển. Khác với nhiều lựa chọn khác, như viết JavaScript thuần túy hoặc sử dụng frameworks như React, Elm là một ngôn ngữ riêng biệt. Với kiểu mạnh và hàm thuần khiết, nó mang lại tính dự đoán và bảo trì.

Khi bạn bắt đầu một dự án Elm mới, bạn cũng đang chấp nhận kiến trúc Elm, một mẫu cho việc cấu trúc các ứng dụng web của bạn nhấn mạnh đến sự đơn giản và khả năng mở rộng. Nó gói gọn trạng thái ứng dụng của bạn và cách nó cập nhật. Các công cụ khác như `create-elm-app` có thể dựng sẵn cấu hình phức tạp hơn, nhưng bắt đầu với `elm init` là cách gọn nhất.

## Xem thêm
- Hướng dẫn Chính thức Elm: https://guide.elm-lang.org/
- Học kiến trúc Elm: https://guide.elm-lang.org/architecture/
- Công cụ Elm: `create-elm-app`: https://github.com/halfzebra/create-elm-app
- Danh mục Gói Elm: https://package.elm-lang.org/

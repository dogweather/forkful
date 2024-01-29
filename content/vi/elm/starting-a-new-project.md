---
title:                "Bắt đầu một dự án mới"
date:                  2024-01-28T22:08:31.270733-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bắt đầu một dự án mới"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc bắt đầu một dự án mới bằng Elm là việc thiết lập một tấm bảng mới để xây dựng các ứng dụng web đáng tin cậy. Các lập trình viên làm điều này để tận dụng sự đơn giản và vững chắc của Elm, đặc biệt là cho các dự án đòi hỏi không có ngoại lệ thời gian chạy.

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

---
title:                "Đọc các đối số dòng lệnh"
aliases:
- /vi/elm/reading-command-line-arguments/
date:                  2024-01-28T22:05:39.961251-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc các đối số dòng lệnh"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Tham số dòng lệnh cho phép người dùng cung cấp dữ liệu cho chương trình của họ khi họ khởi chạy nó. Các lập trình viên đọc các tham số này để điều chỉnh hành vi của chương trình mà không cần phải mã hóa cứng các giá trị.

## Cách thực hiện:
Elm chạy trong trình duyệt, do đó nó không có quyền truy cập trực tiếp vào các tham số dòng lệnh như một ngôn ngữ phía máy chủ hoặc máy để bàn truyền thống. Tuy nhiên, để minh họa, giả sử bạn đang sử dụng Elm với một framework phía máy chủ như Node.js thông qua `elm server` hoặc một cài đặt tương tự cho phép truyền tham số. Mã của bạn sẽ không xử lý trực tiếp các tham số, nhưng chúng tôi sẽ mô phỏng mẫu:

```Elm
-- Giả sử tham số đầu vào từ phía máy chủ
type alias Flags = 
    { arg1 : String
    , arg2 : Int
    }

-- Ví dụ hàm `init` của Elm sử dụng Flags
init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { defaultModel | passedArg1 = flags.arg1, passedArg2 = flags.arg2 }
    , Cmd.none
    )
```

Ví dụ về đầu ra (cấu trúc như nếu được truyền bởi máy chủ):

```JSON
{ "arg1": "Hello", "arg2": 42 }
```

## Đi sâu vào
Vì Elm là một ngôn ngữ frontend, nó truyền thống không xử lý tham số dòng lệnh. Elm hoạt động trong môi trường được kiểm soát của trình duyệt. Dòng lệnh là một di tích từ những ngày điện toán đầu tiên, phục vụ như một cửa sổ vào hệ thống.

Trong Node.js hoặc môi trường tương tự, bạn thường sử dụng `process.argv` để lấy tham số. Với Elm, điều gần nhất bạn có được là cờ (flags) khi khởi tạo ứng dụng Elm từ JavaScript, cho phép tiêm dữ liệu bên ngoài. Bạn chấp nhận gián tiếp các tham số dòng lệnh thông qua ngôn ngữ phía máy chủ, sau đó truyền chúng đến Elm dưới dạng cờ.

Để tích hợp sâu, ứng dụng Elm được gói kèm với mã phía máy chủ, cung cấp trải nghiệm liền mạch cho người dùng. Mô hình khởi động một chương trình Elm với các cờ cụ thể này là mạnh mẽ; nó cho phép khởi động linh hoạt, động sự khởi tạo thích ứng với các môi trường và trường hợp sử dụng khác nhau.

## Xem thêm
- Hướng dẫn chính thức của Elm về flags: https://guide.elm-lang.org/interop/flags.html
- Tài liệu Node.js về tham số dòng lệnh: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- Một ví dụ về Elm với Node.js: https://medium.com/@_rchaves_/using-elm-with-node-elm-server-side-rendering-via-http-nodejs-and-elm-0-19-6c97f062f7eb

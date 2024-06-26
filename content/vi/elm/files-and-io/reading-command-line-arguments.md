---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:39.961251-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Elm ch\u1EA1y trong tr\xECnh duy\u1EC7\
  t, do \u0111\xF3 n\xF3 kh\xF4ng c\xF3 quy\u1EC1n truy c\u1EADp tr\u1EF1c ti\u1EBF\
  p v\xE0o c\xE1c tham s\u1ED1 d\xF2ng l\u1EC7nh nh\u01B0 m\u1ED9t ng\xF4n ng\u1EEF\
  \ ph\xEDa m\xE1y ch\u1EE7 ho\u1EB7c m\xE1y \u0111\u1EC3\u2026"
lastmod: '2024-03-13T22:44:36.565059-06:00'
model: gpt-4-0125-preview
summary: "Elm ch\u1EA1y trong tr\xECnh duy\u1EC7t, do \u0111\xF3 n\xF3 kh\xF4ng c\xF3\
  \ quy\u1EC1n truy c\u1EADp tr\u1EF1c ti\u1EBFp v\xE0o c\xE1c tham s\u1ED1 d\xF2\
  ng l\u1EC7nh nh\u01B0 m\u1ED9t ng\xF4n ng\u1EEF ph\xEDa m\xE1y ch\u1EE7 ho\u1EB7\
  c m\xE1y \u0111\u1EC3 b\xE0n truy\u1EC1n th\u1ED1ng."
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:44.247449-07:00
description: "Vi\u1EBFt l\u1ED7i chu\u1EA9n (stderr) l\xE0 xu\u1EA5t th\xF4ng b\xE1\
  o l\u1ED7i v\xE0 ch\u1EA9n \u0111o\xE1n ri\xEAng bi\u1EC7t kh\u1ECFi \u0111\u1EA7\
  u ra th\xF4ng th\u01B0\u1EDDng. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 g\u1EE1 l\u1ED7i v\xE0 theo\u2026"
lastmod: 2024-02-19 22:04:55.735193
model: gpt-4-0125-preview
summary: "Vi\u1EBFt l\u1ED7i chu\u1EA9n (stderr) l\xE0 xu\u1EA5t th\xF4ng b\xE1o l\u1ED7\
  i v\xE0 ch\u1EA9n \u0111o\xE1n ri\xEAng bi\u1EC7t kh\u1ECFi \u0111\u1EA7u ra th\xF4\
  ng th\u01B0\u1EDDng. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 g\u1EE1 l\u1ED7i v\xE0 theo\u2026"
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
---

{{< edit_this_page >}}

## Gì & Tại sao?
Viết lỗi chuẩn (stderr) là xuất thông báo lỗi và chẩn đoán riêng biệt khỏi đầu ra thông thường. Các lập trình viên thực hiện điều này để gỡ lỗi và theo dõi ứng dụng mà không trộn lẫn thông điệp lỗi với đầu ra chuẩn (stdout).

## Làm như thế nào:
Elm chạy trên web, và trình duyệt không phân biệt giữa stdout và stderr như giao diện dòng lệnh. Tuy nhiên, bạn có thể mô phỏng stderr sử dụng giao tiếp JavaScript thông qua cổng (ports). Đây là cách thiết lập:

```Elm
port module Main exposing (..)

import Html

-- Định nghĩa một cổng để gửi tin nhắn lỗi đến JavaScript
port stderr : String -> Cmd msg

-- Hàm để mô phỏng viết vào stderr
writeToStdErr : String -> Cmd msg
writeToStdErr message =
    stderr message

main =
    writeToStdErr "Lỗi: Đã xảy ra sự cố"
    |> Html.programWithFlags { init = \_ -> ((), Cmd.none), update = \_ _ -> ((), Cmd.none), view = \_ -> Html.text "", subscriptions = \_ -> Sub.none }
```

Và mã JavaScript tương ứng:

```JavaScript
var app = Elm.Main.init();

// Nghe lỗi trên cổng 'stderr' và ghi chúng vào console dưới dạng lỗi
app.ports.stderr.subscribe(function(message) {
    console.error(message);
});
```

Đầu ra mẫu trong console trình duyệt:

```
Lỗi: Đã xảy ra sự cố
```

## Sâu hơn nữa
Theo lịch sử, stderr là một khái niệm của Unix nơi mà luồng đầu ra được phân loại để kiểm soát và tự động hóa quy trình tốt hơn. Elm, vốn chủ yếu là một ngôn ngữ phía frontend, không có sẵn hỗ trợ cho khái niệm này vì ứng dụng web thường xử lý lỗi thông qua UI hoặc qua các hoạt động mạng, không qua terminal. Các phương án thay thế để gỡ lỗi trong Elm bao gồm sử dụng Elm Debugger, mô phỏng trực quan trạng thái của ứng dụng của bạn. Phía sau các cổng, giao tiếp JavaScript của Elm tạo ra các tin nhắn mà JavaScript đăng ký, cầu nối cơ bản giữa Elm và stderr truyền thống.

## Xem thêm
- Hướng dẫn chính thức của Elm về cổng: https://guide.elm-lang.org/interop/ports.html
- Elm Debugger: https://guide.elm-lang.org/effects/debugging.html
- Viết stdout và stderr đa nền tảng trong Node.js: https://nodejs.org/api/console.html

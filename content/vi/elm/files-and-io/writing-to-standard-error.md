---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:44.247449-07:00
description: "L\xE0m nh\u01B0 th\u1EBF n\xE0o: Elm ch\u1EA1y tr\xEAn web, v\xE0 tr\xEC\
  nh duy\u1EC7t kh\xF4ng ph\xE2n bi\u1EC7t gi\u1EEFa stdout v\xE0 stderr nh\u01B0\
  \ giao di\u1EC7n d\xF2ng l\u1EC7nh. Tuy nhi\xEAn, b\u1EA1n c\xF3 th\u1EC3 m\xF4\
  \ ph\u1ECFng stderr s\u1EED\u2026"
lastmod: '2024-03-13T22:44:36.566324-06:00'
model: gpt-4-0125-preview
summary: "Elm ch\u1EA1y tr\xEAn web, v\xE0 tr\xECnh duy\u1EC7t kh\xF4ng ph\xE2n bi\u1EC7\
  t gi\u1EEFa stdout v\xE0 stderr nh\u01B0 giao di\u1EC7n d\xF2ng l\u1EC7nh."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

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

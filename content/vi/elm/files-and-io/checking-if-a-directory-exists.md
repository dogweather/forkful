---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:33.292662-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Elm l\xE0 m\u1ED9t ng\xF4n ng\u1EEF l\u1EAD\
  p tr\xECnh web front-end, v\xEC v\u1EADy n\xF3 kh\xF4ng c\xF3 quy\u1EC1n truy c\u1EAD\
  p tr\u1EF1c ti\u1EBFp v\xE0o h\u1EC7 th\u1ED1ng t\u1EC7p. Tuy nhi\xEAn, b\xECnh\
  \ th\u01B0\u1EDDng b\u1EA1n s\u1EBD\u2026"
lastmod: '2024-03-13T22:44:36.563751-06:00'
model: gpt-4-0125-preview
summary: "Elm l\xE0 m\u1ED9t ng\xF4n ng\u1EEF l\u1EADp tr\xECnh web front-end, v\xEC\
  \ v\u1EADy n\xF3 kh\xF4ng c\xF3 quy\u1EC1n truy c\u1EADp tr\u1EF1c ti\u1EBFp v\xE0\
  o h\u1EC7 th\u1ED1ng t\u1EC7p."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

## Cách thực hiện:
Elm là một ngôn ngữ lập trình web front-end, vì vậy nó không có quyền truy cập trực tiếp vào hệ thống tệp. Tuy nhiên, bình thường bạn sẽ gửi một lệnh cho một dịch vụ phía backend trong JavaScript. Dưới đây là cách bạn có thể cấu trúc một tương tác như vậy với Elm:

```elm
port module Main exposing (..)

-- Định nghĩa một cổng để nói chuyện với JavaScript
port checkDir : String -> Cmd msg

-- Ví dụ sử dụng
checkDirectory : String -> Cmd Msg
checkDirectory dir =
    checkDir dir
```

Sau đó, trong JavaScript của bạn:

```javascript
app.ports.checkDir.subscribe(function(dir) {
    var exists = fs.existsSync(dir); // Sử dụng module 'fs' của Node để kiểm tra thư mục
    app.ports.dirExists.send(exists);
});
```

Quay lại với Elm, xử lý phản hồi:

```elm
port dirExists : (Bool -> msg) -> Sub msg

type Msg = DirExists Bool

subscriptions : Model -> Sub Msg
subscriptions model =
    dirExists DirExists
```

Lưu ý: Điều này đòi hỏi thiết lập cổng và xử lý phía backend thích hợp trong JavaScript.

## Đi sâu hơn
Môi trường giới hạn trong trình duyệt của Elm có nghĩa là nó không thể truy cập trực tiếp vào hệ thống tệp, không giống như Node.js. Truyền thống, các ngôn ngữ phía server và Node.js đã cung cấp chức năng truy cập hệ thống tệp, với các ngôn ngữ trên trình duyệt dựa vào API server để quản lý tệp. Hệ thống kiểu nghiêm ngặt của Elm không quản lý trực tiếp các hiệu ứng phụ như các hoạt động I/O; thay vào đó, nó sử dụng cổng cho sự tương tác với JavaScript. Mặc dù chính Elm không thể kiểm tra xem một thư mục có tồn tại không, việc sử dụng Elm với một dịch vụ phía backend qua cổng cho phép thực hiện chức năng này trong các ứng dụng web.

Các phương án thay thế trong môi trường Node.js bao gồm các phương thức `fs.existsSync` hoặc `fs.access`. Đối với Elm, xem xét Elm phía server với một backend như `elm-serverless` có thể xử lý các hoạt động tệp một cách trực tiếp hơn so với Elm phía khách hàng.

Về mặt triển khai, một khi bạn đã thiết lập cổng của mình, ứng dụng Elm của bạn gửi thông điệp đến JavaScript, cái mà thực hiện việc kiểm tra hệ thống tệp. JavaScript sau đó gửi kết quả trở lại cho Elm. Điều này giữ cho mã front-end của Elm trong sạch và tự do từ các hiệu ứng phụ, giữ nguyên các nguyên tắc kiến trúc của nó.

## Xem thêm
- Hướng dẫn Chính thức về Cổng của Elm: https://guide.elm-lang.org/interop/ports.html
- Tài liệu module `fs` của Node.js: https://nodejs.org/api/fs.html
- elm-serverless cho các tương tác Elm phía server: https://package.elm-lang.org/packages/ktonon/elm-serverless/latest/

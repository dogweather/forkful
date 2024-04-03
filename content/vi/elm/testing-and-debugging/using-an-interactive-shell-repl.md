---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:31.422852-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Elm kh\xF4ng \u0111i k\xE8m v\u1EDBi REPL\
  \ t\xEDch h\u1EE3p. Tuy nhi\xEAn, b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng `elm\
  \ repl` t\u1EEB d\xF2ng l\u1EC7nh c\u1EE7a m\xECnh \u0111\u1EC3 b\u1EAFt \u0111\u1EA7\
  u m\u1ED9t phi\xEAn Elm sau khi \u0111\xE3 c\xE0i\u2026"
lastmod: '2024-03-13T22:44:36.546440-06:00'
model: gpt-4-0125-preview
summary: "Elm kh\xF4ng \u0111i k\xE8m v\u1EDBi REPL t\xEDch h\u1EE3p."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

## Cách thực hiện:
Elm không đi kèm với REPL tích hợp. Tuy nhiên, bạn có thể sử dụng `elm repl` từ dòng lệnh của mình để bắt đầu một phiên Elm sau khi đã cài đặt Elm.

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

Trong phiên này, sau khi nhập các hàm List, chúng tôi đã nhân đôi các số trong một danh sách và nhận kết quả ngay lập tức.

## Sâu hơn nữa
REPL của Elm có thể có vẻ hạn chế so với REPL của một số ngôn ngữ khác như Python hoặc JavaScript, do Elm là một ngôn ngữ biên dịch tập trung vào việc sản xuất ứng dụng web. Lịch sử, Elm tập trung vào các ứng dụng đầy đủ hơn là lập kịch bản hoặc tương tác shell.

Các phương án thay thế cho REPL của Elm bao gồm `elm-live` và các trình biên tập trực tuyến như Ellie, nơi bạn có thể thấy các thay đổi đối với mã được phản ánh vào thời gian thực trong một trình duyệt.

Liên quan đến triển khai, REPL của Elm biên dịch các đoạn mã Elm thành JavaScript ở hậu trường, cho phép bạn chạy Elm một cách tương tác. Điều này khác với REPL của các ngôn ngữ được giải thích, không cần bước biên dịch này. REPL của Elm cũng được cắt giảm để giữ cho ngôn ngữ cốt lõi nhẹ và tập trung.

## Xem thêm
- Hướng dẫn chính thức về giao tiếp của Elm: https://guide.elm-lang.org/interop/
- Ellie, một sân chơi Elm trực tuyến: https://ellie-app.com/new
- `elm-live`, một máy chủ phát triển linh hoạt cho Elm: https://www.elm-live.com/

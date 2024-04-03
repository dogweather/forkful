---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:31.422852-07:00
description: "V\xF2ng l\u1EB7p \u0110\u1ECDc-\u0110\xE1nh gi\xE1-In (REPL) l\xE0 m\u1ED9\
  t m\xF4i tr\u01B0\u1EDDng l\u1EADp tr\xECnh t\u01B0\u01A1ng t\xE1c \u0111\u01A1\
  n gi\u1EA3n, x\u1EED l\xFD t\u1EEBng \u0111\u1EA7u v\xE0o c\u1EE7a ng\u01B0\u1EDD\
  i d\xF9ng, \u0111\xE1nh gi\xE1 ch\xFAng, v\xE0 tr\u1EA3 k\u1EBFt qu\u1EA3 v\u1EC1\
  \ cho\u2026"
lastmod: '2024-03-13T22:44:36.546440-06:00'
model: gpt-4-0125-preview
summary: "V\xF2ng l\u1EB7p \u0110\u1ECDc-\u0110\xE1nh gi\xE1-In (REPL) l\xE0 m\u1ED9\
  t m\xF4i tr\u01B0\u1EDDng l\u1EADp tr\xECnh t\u01B0\u01A1ng t\xE1c \u0111\u01A1\
  n gi\u1EA3n, x\u1EED l\xFD t\u1EEBng \u0111\u1EA7u v\xE0o c\u1EE7a ng\u01B0\u1EDD\
  i d\xF9ng, \u0111\xE1nh gi\xE1 ch\xFAng, v\xE0 tr\u1EA3 k\u1EBFt qu\u1EA3 v\u1EC1\
  \ cho ng\u01B0\u1EDDi d\xF9ng."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

## Cái gì & Tại sao?

Vòng lặp Đọc-Đánh giá-In (REPL) là một môi trường lập trình tương tác đơn giản, xử lý từng đầu vào của người dùng, đánh giá chúng, và trả kết quả về cho người dùng. Lập trình viên Elm sử dụng REPL để thử nghiệm nhanh, gỡ lỗi, hoặc học ngôn ngữ.

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

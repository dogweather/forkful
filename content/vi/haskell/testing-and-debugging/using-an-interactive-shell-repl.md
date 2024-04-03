---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:10.662195-07:00
description: "M\u1ED9t shell t\u01B0\u01A1ng t\xE1c, hay REPL (Read-Eval-Print Loop),\
  \ trong Haskell cho ph\xE9p b\u1EA1n ch\u1EA1y c\xE1c \u0111o\u1EA1n m\xE3 tr\u1EF1\
  c ti\u1EBFp. \u0110\xF3 l\xE0 s\xE2n ch\u01A1i cho ph\u1EA3n h\u1ED3i nhanh ch\xF3\
  ng, ki\u1EC3m\u2026"
lastmod: '2024-03-13T22:44:36.714191-06:00'
model: gpt-4-0125-preview
summary: "M\u1ED9t shell t\u01B0\u01A1ng t\xE1c, hay REPL (Read-Eval-Print Loop),\
  \ trong Haskell cho ph\xE9p b\u1EA1n ch\u1EA1y c\xE1c \u0111o\u1EA1n m\xE3 tr\u1EF1\
  c ti\u1EBFp."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

## Cái gì & Tại sao?
Một shell tương tác, hay REPL (Read-Eval-Print Loop), trong Haskell cho phép bạn chạy các đoạn mã trực tiếp. Đó là sân chơi cho phản hồi nhanh chóng, kiểm thử các hàm, và học ngôn ngữ.

## Làm thế nào:
Để bắt đầu môi trường tương tác GHCi (Glasgow Haskell Compiler's interactive environment), chỉ cần gõ `ghci` trong terminal của bạn. Dưới đây là cách sử dụng nó:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

Đầu ra mẫu giải thích rằng `x` là một biến số và cho thấy việc nhân đôi nó sẽ ra kết quả là 10.

## Đào Sâu:
Môi trường GHCi của Haskell đã phát triển rất nhiều kể từ khi nó được tạo ra. Nó cung cấp một bộ tính năng phong phú như hoàn thành tự động bằng tab, đầu vào nhiều dòng, và tải gói. Các lựa chọn thay thế như Hugs hiện nay chủ yếu là lịch sử, với GHCi là tiêu chuẩn. GHCi biên dịch mã nguồn ngay lập tức mỗi khi bạn nhập một biểu thức, mang lại cho bạn một cách hiệu quả để kiểm thử mã Haskell của bạn.

## Xem Thêm:
- [Hướng dẫn Sử dụng GHC – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Learn You a Haskell for Great Good! – Bắt đầu](http://learnyouahaskell.com/starting-out#hello-world)
- [Wiki Haskell – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)

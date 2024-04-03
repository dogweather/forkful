---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:10.662195-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 b\u1EAFt \u0111\u1EA7u m\xF4i tr\u01B0\
  \u1EDDng t\u01B0\u01A1ng t\xE1c GHCi (Glasgow Haskell Compiler's interactive environment),\
  \ ch\u1EC9 c\u1EA7n g\xF5 `ghci` trong terminal c\u1EE7a b\u1EA1n. D\u01B0\u1EDB\
  i \u0111\xE2y\u2026"
lastmod: '2024-03-13T22:44:36.714191-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 b\u1EAFt \u0111\u1EA7u m\xF4i tr\u01B0\u1EDDng t\u01B0\u01A1\
  ng t\xE1c GHCi (Glasgow Haskell Compiler's interactive environment), ch\u1EC9 c\u1EA7\
  n g\xF5 `ghci` trong terminal c\u1EE7a b\u1EA1n."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

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

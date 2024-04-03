---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:32.984541-07:00
description: "C\xE1ch th\u1EE9c: Tri\u1EBFt l\xFD c\u1ED1t l\xF5i c\u1EE7a Elm l\xE0\
  \ Kh\xF4ng C\xF3 Ngo\u1EA1i L\u1EC7 Th\u1EDDi Gian Ch\u1EA1y. V\xEC th\u1EBF, Elm\
  \ t\u1EADn d\u1EE5ng h\u1EC7 th\u1ED1ng ki\u1EC3u c\u1EE7a m\xECnh v\u1EDBi nh\u1EEF\
  ng ki\u1EC3u nh\u01B0 `Maybe` v\xE0\u2026"
lastmod: '2024-03-13T22:44:36.554594-06:00'
model: gpt-4-0125-preview
summary: "Tri\u1EBFt l\xFD c\u1ED1t l\xF5i c\u1EE7a Elm l\xE0 Kh\xF4ng C\xF3 Ngo\u1EA1\
  i L\u1EC7 Th\u1EDDi Gian Ch\u1EA1y."
title: "X\u1EED l\xFD l\u1ED7i"
weight: 16
---

## Cách thức:
Triết lý cốt lõi của Elm là Không Có Ngoại Lệ Thời Gian Chạy. Vì thế, Elm tận dụng hệ thống kiểu của mình với những kiểu như `Maybe` và `Result` để xử lý lỗi.

Với trường hợp `Maybe`:

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    if denominator == 0 then
        Nothing
    else
        Just (numerator / denominator)
        
-- Khi bạn chạy nó:

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

Với trường hợp `Result`:

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide numerator denominator =
    if denominator == 0 then
        Err DivisionByZero
    else
        Ok (numerator / denominator)

-- Và sử dụng nó:

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

## Sâu hơn nữa
Hệ thống kiểu của Elm là nghiêm ngặt, giúp phát hiện lỗi sớm. Trong lịch sử, hầu hết các ngôn ngữ dựa vào ngoại lệ và kiểm tra thời gian chạy, nhưng Elm chọn đảm bảo tại thời điểm biên dịch. Các lựa chọn khác như `Result` cho phép thông tin lỗi chi tiết, trong khi `Maybe` đơn giản hơn cho các tình huống có-không. Cách xử lý lỗi của Elm khuyến khích các nhà phát triển xem xét tất cả các lối đi ngay từ đầu, tránh được cái bẫy của việc quên mất các trường hợp lỗi.

## Xem thêm:
- Phần hướng dẫn chính thức của Elm về xử lý lỗi: [Xử Lý Lỗi – Giới Thiệu](https://guide.elm-lang.org/error_handling/)
- Tài liệu Elm về `Maybe`: [Elm – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- Tài liệu Elm về `Result`: [Elm – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)

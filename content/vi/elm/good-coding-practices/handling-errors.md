---
title:                "Xử lý lỗi"
aliases:
- /vi/elm/handling-errors.md
date:                  2024-01-28T22:01:32.984541-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xử lý lỗi"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Lý do & Tại sao?
Xử lý lỗi có nghĩa là viết mã có thể dự đoán và xử lý những điều không diễn ra như mong đợi. Lập trình viên thực hiện điều này để ngăn chặn sự cố, bảo vệ tính toàn vẹn của dữ liệu và cung cấp cho người dùng những phương án thay thế một cách duyên dáng.

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

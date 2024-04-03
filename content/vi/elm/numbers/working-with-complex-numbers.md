---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:31.009790-07:00
description: "L\xE0m th\u1EBF n\xE0o: Elm kh\xF4ng h\u1ED7 tr\u1EE3 s\u1ED1 ph\u1EE9\
  c m\u1ED9t c\xE1ch s\u1EB5n c\xF3, v\xEC v\u1EADy b\u1EA1n s\u1EBD t\u1EF1 t\u1EA1\
  o ki\u1EC3u d\u1EEF li\u1EC7u v\xE0 c\xE1c h\xE0m c\u1EE7a ri\xEAng m\xECnh. D\u01B0\
  \u1EDBi \u0111\xE2y l\xE0 m\u1ED9t c\xE1ch thi\u1EBFt l\u1EADp nhanh."
lastmod: '2024-03-13T22:44:36.535824-06:00'
model: gpt-4-0125-preview
summary: "Elm kh\xF4ng h\u1ED7 tr\u1EE3 s\u1ED1 ph\u1EE9c m\u1ED9t c\xE1ch s\u1EB5\
  n c\xF3, v\xEC v\u1EADy b\u1EA1n s\u1EBD t\u1EF1 t\u1EA1o ki\u1EC3u d\u1EEF li\u1EC7\
  u v\xE0 c\xE1c h\xE0m c\u1EE7a ri\xEAng m\xECnh."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

## Làm thế nào:
Elm không hỗ trợ số phức một cách sẵn có, vì vậy bạn sẽ tự tạo kiểu dữ liệu và các hàm của riêng mình. Dưới đây là một cách thiết lập nhanh:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- Ví dụ sử dụng:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum là { real = 4.0, imaginary = -2.0 }
```

## Sâu hơn
Về mặt lịch sử, số phức không luôn được chấp nhận. Chúng trở thành yếu tố thay đổi cuộc chơi trong thế kỷ 16 để giải các phương trình bậc ba. Các ngôn ngữ khác như Python cung cấp hỗ trợ số phức sẵn có với các phép toán ngay khi ra khỏi hộp. Elm yêu cầu một cách tiếp cận tự làm như bạn đã thấy. Nhưng bạn có thể làm cho nó phức tạp như mình cần, xây dựng phép nhân, phép chia và các phép toán khác, điều chỉnh vấn đề hiệu suất.

## Xem thêm
- Tài liệu chính thức của Elm: https://package.elm-lang.org/ để tạo các kiểu tùy chỉnh và nắm vững cơ bản của Elm.
- Những người đam mê lịch sử toán học có thể tham khảo "An Imaginary Tale" của Paul J. Nahin để biết hành trình qua thời gian của số phức.
- Tham gia vào thử thách lập trình với định hướng toán học trên Project Euler (https://projecteuler.net) để áp dụng sự hiểu biết về số phức của bạn.

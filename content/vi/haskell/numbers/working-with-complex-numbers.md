---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:59.170029-07:00
description: "L\xE0m th\u1EBF n\xE0o: Haskell x\u1EED l\xFD s\u1ED1 ph\u1EE9c v\u1EDB\
  i module `Data.Complex`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t h\u01B0\u1EDB\
  ng d\u1EABn nhanh."
lastmod: '2024-03-13T22:44:36.704549-06:00'
model: gpt-4-0125-preview
summary: "Haskell x\u1EED l\xFD s\u1ED1 ph\u1EE9c v\u1EDBi module `Data.Complex`."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

## Làm thế nào:
Haskell xử lý số phức với module `Data.Complex`. Dưới đây là một hướng dẫn nhanh:

```haskell
import Data.Complex

-- Định nghĩa hai số phức
let z1 = 3 :+ 4  -- đó là 3 + 4i
let z2 = 5 :+ (-2)  -- 5 - 2i

-- Các phép toán cộng trừ nhân chia
let sum = z1 + z2  -- 8 :+ 2
let difference = z1 - z2  -- -2 :+ 6
let product = z1 * z2  -- 23 :+ 14
let quotient = z1 / z2  -- 0.20689655172413793 :+ 0.9655172413793104

-- Liên hợp phức
let conjugateZ1 = conjugate z1  -- 3 :+ (-4)

-- Độ lớn và pha
let magnitudeZ1 = magnitude z1  -- 5.0
let phaseZ1 = phase z1  -- 0.9272952180016122

-- Chuyển đổi từ cực sang chữ nhật và ngược lại
let z1Polar = polar z1  -- (5.0,0.9272952180016122)
let fromPolar = mkPolar 5.0 0.9272952180016122  -- giống như z1
```

Kết quả mẫu sau khi tải mã trên trong GHCi có thể là:

```haskell
*Main> sum
8.0 :+ 2.0
*Main> product
23.0 :+ 14.0
*Main> magnitudeZ1
5.0
```

## Sâu hơn
Số phức có từ thế kỷ 16 nhưng được chấp nhận rộng rãi nhiều sau đó. Haskell, giống như nhiều ngôn ngữ khác, cung cấp hỗ trợ bản địa cho các phép toán số phức, làm cho việc làm việc với những số này không cần phải thực hiện các phép toán cơ bản.

Các lựa chọn thay thế bao gồm việc tạo kiểu số phức tùy chỉnh của riêng bạn hoặc sử dụng thư viện cho các miền cụ thể như quaternion cho đồ họa 3D. Nhưng cho hầu hết các trường hợp sử dụng, `Data.Complex` của Haskell là đủ.

Bên dưới `Data.Complex` chỉ là kiểu dữ liệu ghép đôi hai giá trị `Float` hoặc `Double`, đại diện cho phần thực và phần ảo, tương ứng. Đây là một cách đơn giản và hiệu quả để làm việc với số phức trên nền tảng Haskell.

## Xem thêm
Xem những tài nguyên sau để biết thêm về số phức trong Haskell:

- Tài liệu chính thức của Haskell `Data.Complex`: [Hackage Data.Complex](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Complex.html)
- Sâu hơn về các kiểu số của Haskell: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/starting-out#numbers)
- Đối với một ứng dụng, khám phá thuật toán Biến đổi Fourier Nhanh trong Haskell: [Thư viện FFT Haskell](https://hackage.haskell.org/package/fft)

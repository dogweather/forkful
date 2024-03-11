---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:59.170029-07:00
description: "S\u1ED1 ph\u1EE9c, bao g\u1ED3m m\u1ED9t ph\u1EA7n th\u1EF1c v\xE0 m\u1ED9\
  t ph\u1EA7n \u1EA3o, l\xE0 c\u1EA7n thi\u1EBFt trong c\xE1c l\u0129nh v\u1EF1c t\xED\
  nh to\xE1n kh\xE1c nhau nh\u01B0 k\u1EF9 thu\u1EADt, v\u1EADt l\xFD, v\xE0 x\u1EED\
  \ l\xFD t\xEDn hi\u1EC7u. L\u1EADp tr\xECnh vi\xEAn\u2026"
lastmod: '2024-03-11T00:14:09.983197-06:00'
model: gpt-4-0125-preview
summary: "S\u1ED1 ph\u1EE9c, bao g\u1ED3m m\u1ED9t ph\u1EA7n th\u1EF1c v\xE0 m\u1ED9\
  t ph\u1EA7n \u1EA3o, l\xE0 c\u1EA7n thi\u1EBFt trong c\xE1c l\u0129nh v\u1EF1c t\xED\
  nh to\xE1n kh\xE1c nhau nh\u01B0 k\u1EF9 thu\u1EADt, v\u1EADt l\xFD, v\xE0 x\u1EED\
  \ l\xFD t\xEDn hi\u1EC7u. L\u1EADp tr\xECnh vi\xEAn\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Số phức, bao gồm một phần thực và một phần ảo, là cần thiết trong các lĩnh vực tính toán khác nhau như kỹ thuật, vật lý, và xử lý tín hiệu. Lập trình viên sử dụng chúng để giải quyết các phương trình mà số thực không thể, như tìm căn bậc hai của số âm.

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

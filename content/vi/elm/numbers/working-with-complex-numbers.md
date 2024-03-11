---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:31.009790-07:00
description: "S\u1ED1 ph\u1EE9c l\xE0 s\u1EF1 k\u1EBFt h\u1EE3p c\u1EE7a s\u1ED1 th\u1EF1\
  c v\xE0 s\u1ED1 \u1EA3o, v\xED d\u1EE5 nh\u01B0 `a + bi` n\u01A1i `i` l\xE0 c\u0103\
  n b\u1EADc hai c\u1EE7a -1. Ch\xFAng l\xE0 ch\xECa kh\xF3a trong c\xE1c l\u0129\
  nh v\u1EF1c nh\u01B0 k\u1EF9 thu\u1EADt v\xE0 v\u1EADt l\xFD\u2026"
lastmod: '2024-03-11T00:14:09.804602-06:00'
model: gpt-4-0125-preview
summary: "S\u1ED1 ph\u1EE9c l\xE0 s\u1EF1 k\u1EBFt h\u1EE3p c\u1EE7a s\u1ED1 th\u1EF1\
  c v\xE0 s\u1ED1 \u1EA3o, v\xED d\u1EE5 nh\u01B0 `a + bi` n\u01A1i `i` l\xE0 c\u0103\
  n b\u1EADc hai c\u1EE7a -1. Ch\xFAng l\xE0 ch\xECa kh\xF3a trong c\xE1c l\u0129\
  nh v\u1EF1c nh\u01B0 k\u1EF9 thu\u1EADt v\xE0 v\u1EADt l\xFD\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Số phức là sự kết hợp của số thực và số ảo, ví dụ như `a + bi` nơi `i` là căn bậc hai của -1. Chúng là chìa khóa trong các lĩnh vực như kỹ thuật và vật lý để giải quyết những bài toán mà số thường không thể chạm tới.

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

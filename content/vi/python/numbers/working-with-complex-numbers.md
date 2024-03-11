---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:20.424454-07:00
description: "S\u1ED1 ph\u1EE9c l\xE0 m\u1ED9t t\u1EADp h\u1EE3p c\xE1c s\u1ED1 c\xF3\
  \ d\u1EA1ng `a + bi`, n\u01A1i `a` v\xE0 `b` l\xE0 c\xE1c s\u1ED1 th\u1EF1c, v\xE0\
  \ `i` l\xE0 \u0111\u01A1n v\u1ECB \u1EA3o (`i^2 = -1`). Trong l\u1EADp tr\xECnh,\
  \ ch\xFAng ta s\u1EED d\u1EE5ng ch\xFAng\u2026"
lastmod: '2024-03-11T00:14:09.319172-06:00'
model: gpt-4-0125-preview
summary: "S\u1ED1 ph\u1EE9c l\xE0 m\u1ED9t t\u1EADp h\u1EE3p c\xE1c s\u1ED1 c\xF3\
  \ d\u1EA1ng `a + bi`, n\u01A1i `a` v\xE0 `b` l\xE0 c\xE1c s\u1ED1 th\u1EF1c, v\xE0\
  \ `i` l\xE0 \u0111\u01A1n v\u1ECB \u1EA3o (`i^2 = -1`). Trong l\u1EADp tr\xECnh,\
  \ ch\xFAng ta s\u1EED d\u1EE5ng ch\xFAng\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Số phức là một tập hợp các số có dạng `a + bi`, nơi `a` và `b` là các số thực, và `i` là đơn vị ảo (`i^2 = -1`). Trong lập trình, chúng ta sử dụng chúng để giải quyết các vấn đề trong nhiều lĩnh vực, như kỹ thuật điện, xử lý tín hiệu và tính toán lượng tử.

## Làm thế nào:
Python hỗ trợ sẵn số phức. Dưới đây là cách bạn có thể thao tác với chúng:

```Python
# Tạo số phức
z = 4 + 5j
print(z)  # Đầu ra: (4+5j)

# Truy cập phần thực và phần ảo
print(z.real)  # Đầu ra: 4.0
print(z.imag)  # Đầu ra: 5.0

# Phép toán số phức
w = 1 - 2j
print(z + w)  # Đầu ra: (5+3j)
print(z - w)  # Đầu ra: (3+7j)
print(z * w)  # Đầu ra: (14+2j)
print(z / w)  # Đầu ra: (-3.6+1.2j)

# Modulus (giá trị tuyệt đối)
print(abs(z))  # Đầu ra: 6.4031242374328485

# Số phức liên hợp
print(z.conjugate())  # Đầu ra: (4-5j)
```

## Sâu hơn
Số phức được Girolamo Cardano lần đầu tiên khái niệm hóa vào thế kỷ 16. Python, cùng với các ngôn ngữ lập trình khác, đối xử với số phức như những công dân hạng nhất. Điều này có nghĩa là chúng được xây dựng sẵn trong ngôn ngữ, với các tính năng dễ sử dụng, tránh cần phải nhập các thư viện bên ngoài cho các thao tác cơ bản.

Tuy nhiên, cho các phép tính số học nặng, Python có một thư viện gọi là `cmath`, chuyên dành cho số phức. Nó có các hàm bổ sung như `exp`, `log`, và các phép toán lượng giác.

Khi Python không đủ, bạn có thể chuyển sang các thư viện như NumPy, đặc biệt là cho các thao tác mảng liên quan đến số phức. NumPy cung cấp các thao tác tối ưu hóa và vector hóa quan trọng cho hiệu suất trong tính toán số học.

## Xem thêm
Hãy xem các nguồn lực này để tìm hiểu thêm:

- Tài liệu chính thức của Python về số phức: https://docs.python.org/3/library/stdtypes.html#typesnumeric
- Tài liệu về mô-đun `cmath`: https://docs.python.org/3/library/cmath.html
- NumPy cho xử lý mảng số phức: https://numpy.org/doc/stable/user/absolute_beginners.html#the-basics

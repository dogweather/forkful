---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:20.424454-07:00
description: "L\xE0m th\u1EBF n\xE0o: Python h\u1ED7 tr\u1EE3 s\u1EB5n s\u1ED1 ph\u1EE9\
  c. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n c\xF3 th\u1EC3 thao t\xE1c\
  \ v\u1EDBi ch\xFAng."
lastmod: '2024-03-13T22:44:36.088639-06:00'
model: gpt-4-0125-preview
summary: "Python h\u1ED7 tr\u1EE3 s\u1EB5n s\u1ED1 ph\u1EE9c."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

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

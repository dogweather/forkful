---
title:                "Làm việc với số phức"
date:                  2024-01-28T22:13:20.424454-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"

category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

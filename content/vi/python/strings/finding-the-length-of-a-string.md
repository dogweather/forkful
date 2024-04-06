---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:45.484097-07:00
description: "C\xE1ch l\xE0m: Trong l\u1ECBch s\u1EED, h\xE0m `len()` lu\xF4n l\xE0\
  \ c\xE1ch \u0111i t\u1EDBi c\u1EE7a Python \u0111\u1EC3 t\xECm chi\u1EC1u d\xE0\
  i c\u1EE7a m\u1ED9t chu\u1ED7i. N\xF3 tinh t\u1EBF v\xE0 nhanh ch\xF3ng. B\xEAn\
  \ d\u01B0\u1EDBi, chu\u1ED7i Python l\xE0\u2026"
lastmod: '2024-04-05T21:53:37.516587-06:00'
model: gpt-4-0125-preview
summary: "Trong l\u1ECBch s\u1EED, h\xE0m `len()` lu\xF4n l\xE0 c\xE1ch \u0111i t\u1EDB\
  i c\u1EE7a Python \u0111\u1EC3 t\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7\
  i."
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

## Cách làm:
```python
# Sử dụng đơn giản hàm len()
my_string = "Hello, World!"
length = len(my_string)
print(length)  # Kết quả: 13

# Độ dài trong một vòng lặp
for i in range(len(my_string)):
    print(my_string[i], end='')  # Kết quả: Hello, World!
print()  # Để xuống dòng

# Kết hợp độ dài chuỗi với các thao tác khác
if len(my_string) > 10:
    print("Đây là một chuỗi dài!")  # Kết quả: Đây là một chuỗi dài!
```

## Sâu hơn
Trong lịch sử, hàm `len()` luôn là cách đi tới của Python để tìm chiều dài của một chuỗi. Nó tinh tế và nhanh chóng. Bên dưới, chuỗi Python là các mảng byte đại diện cho các ký tự Unicode, và `len()` đếm số byte đó. Hàm này không chỉ làm việc với chuỗi mà còn với bất kỳ loại lặp nào.

Có phương án thay thế không? Chà, không thường được sử dụng cho chuỗi, nhưng bạn có thể lặp qua một chuỗi và đếm ký tự một cách thủ công - cồng kềnh và kém hiệu quả. Trước khi hỗ trợ Unicode, đôi khi chiều dài của một chuỗi khác biệt với kích thước bộ nhớ của nó, nhưng kể từ khi chuỗi trong Python 3 là bản địa Unicode, `len()` chính xác đại diện cho số lượng ký tự.

Về mặt triển khai, chuỗi Python là các đối tượng với siêu dữ liệu, bao gồm chiều dài, vì vậy `len()` thực sự là một thao tác O(1) - thời gian cố định, bất kể kích thước của chuỗi. Đó giống như chớp mắt và nhận được câu trả lời.

## Xem thêm
- Tài liệu Python cho `len()`: https://docs.python.org/3/library/functions.html#len
- Unicode và Mã hóa Chuỗi trong Python: https://docs.python.org/3/howto/unicode.html
- Độ phức tạp thời gian của Python cho các kiểu có sẵn: https://wiki.python.org/moin/TimeComplexity

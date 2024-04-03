---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:45.484097-07:00
description: "Vi\u1EC7c t\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i bao\
  \ g\u1ED3m vi\u1EC7c \u0111\u1EBFm s\u1ED1 k\xFD t\u1EF1 c\u1EE7a n\xF3. L\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 x\xE1c th\u1EF1\
  c \u0111\u1EA7u v\xE0o, l\u1EB7p qua c\xE1c chu\u1ED7i, ph\xE2n b\u1ED5 ngu\u1ED3\
  n\u2026"
lastmod: '2024-03-13T22:44:36.084817-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i bao g\u1ED3\
  m vi\u1EC7c \u0111\u1EBFm s\u1ED1 k\xFD t\u1EF1 c\u1EE7a n\xF3."
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

## Gì và Tại sao?

Việc tìm chiều dài của một chuỗi bao gồm việc đếm số ký tự của nó. Lập trình viên thực hiện việc này để xác thực đầu vào, lặp qua các chuỗi, phân bổ nguồn lực, cùng một số nhiệm vụ khác.

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

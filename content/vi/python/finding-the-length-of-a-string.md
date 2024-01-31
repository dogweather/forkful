---
title:                "Tìm chiều dài của một chuỗi ký tự"
date:                  2024-01-28T22:00:45.484097-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

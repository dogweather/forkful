---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:03.070048-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: ."
lastmod: '2024-03-13T22:44:36.082298-06:00'
model: gpt-4-0125-preview
summary: .
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Cách thực hiện:
```Python
# Sử dụng ký hiệu cắt
text = "Python rocks!"
substring = text[7:12]
print(substring)  # Đầu ra: rocks

# Sử dụng hàm slice()
slice_object = slice(7, 12)
print(text[slice_object])  # Đầu ra: rocks

# Sử dụng str.split() và truy cập phần tử
parts = text.split()
print(parts[1])  # Đầu ra: rocks!
```

## Tìm hiểu sâu
Về mặt lịch sử, khái niệm về thao tác chuỗi, bao gồm cả việc trích xuất chuỗi con, đã rất quan trọng trong các ngôn ngữ lập trình đầu tiên như C, nơi đó là một nhiệm vụ phức tạp hơn liên quan đến các con trỏ. Với Python, sự đơn giản được nâng lên tới mức mười một - trực quan hơn và ít lỗi hơn.

Python cung cấp nhiều phương án thay thế để trích xuất chuỗi con. Mặc dù các ví dụ sử dụng ký hiệu cắt rất trực tiếp, các phương thức như `split()` có thể hữu ích khi bạn đang xử lý với các dấu phân cách hoặc khoảng trắng.

Bên dưới lớp vỏ, chuỗi Python là các mảng byte biểu diễn các ký tự Unicode. Nhưng không giống như mảng trong các ngôn ngữ khác, chuỗi Python là bất biến, tức là bạn không thể thay đổi chúng sau khi tạo. Khía cạnh này là thiết yếu khi hiểu tại sao các hoạt động trích xuất chuỗi con không chỉnh sửa chuỗi gốc mà thay vào đó tạo ra một mới.

## Xem thêm
- Tài liệu Python về các phương thức chuỗi: https://docs.python.org/3/library/stdtypes.html#string-methods
- Một bài viết về các thao tác chuỗi Python khác: https://realpython.com/python-strings/
- Hướng dẫn cắt chuỗi Python của W3Schools: https://www.w3schools.com/python/python_strings_slicing.asp

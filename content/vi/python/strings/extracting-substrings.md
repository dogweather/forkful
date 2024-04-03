---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:03.070048-07:00
description: "Tr\xEDch xu\u1EA5t chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 l\u1EA5y ra\
  \ c\xE1c ph\u1EA7n c\u1EE5 th\u1EC3 c\u1EE7a m\u1ED9t chu\u1ED7i, gi\u1ED1ng nh\u01B0\
  \ c\u1EAFt m\u1ED9t m\u1EA3nh t\u1EEB m\u1ED9t s\u1EE3i ruy b\u0103ng. L\u1EADp\
  \ tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\xE1ch d\u1EEF\u2026"
lastmod: '2024-03-13T22:44:36.082298-06:00'
model: gpt-4-0125-preview
summary: "Tr\xEDch xu\u1EA5t chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 l\u1EA5y ra c\xE1\
  c ph\u1EA7n c\u1EE5 th\u1EC3 c\u1EE7a m\u1ED9t chu\u1ED7i, gi\u1ED1ng nh\u01B0 c\u1EAF\
  t m\u1ED9t m\u1EA3nh t\u1EEB m\u1ED9t s\u1EE3i ruy b\u0103ng."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Gì & Tại sao?
Trích xuất chuỗi con có nghĩa là lấy ra các phần cụ thể của một chuỗi, giống như cắt một mảnh từ một sợi ruy băng. Lập trình viên làm điều này để tách dữ liệu, phân tích thông tin, hoặc đơn giản chỉ là thao tác với văn bản.

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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:03.070048-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: V\u1EC1 m\u1EB7t l\u1ECBch s\u1EED, kh\xE1\
  i ni\u1EC7m v\u1EC1 thao t\xE1c chu\u1ED7i, bao g\u1ED3m c\u1EA3 vi\u1EC7c tr\xED\
  ch xu\u1EA5t chu\u1ED7i con, \u0111\xE3 r\u1EA5t quan tr\u1ECDng trong c\xE1c ng\xF4\
  n ng\u1EEF l\u1EADp tr\xECnh \u0111\u1EA7u ti\xEAn\u2026"
lastmod: '2024-04-05T21:53:37.514099-06:00'
model: gpt-4-0125-preview
summary: "V\u1EC1 m\u1EB7t l\u1ECBch s\u1EED, kh\xE1i ni\u1EC7m v\u1EC1 thao t\xE1\
  c chu\u1ED7i, bao g\u1ED3m c\u1EA3 vi\u1EC7c tr\xEDch xu\u1EA5t chu\u1ED7i con,\
  \ \u0111\xE3 r\u1EA5t quan tr\u1ECDng trong c\xE1c ng\xF4n ng\u1EEF l\u1EADp tr\xEC\
  nh \u0111\u1EA7u ti\xEAn nh\u01B0 C, n\u01A1i \u0111\xF3 l\xE0 m\u1ED9t nhi\u1EC7\
  m v\u1EE5 ph\u1EE9c t\u1EA1p h\u01A1n li\xEAn quan \u0111\u1EBFn c\xE1c con tr\u1ECF\
  ."
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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:37.723176-07:00
description: "L\xE0m th\u1EBF n\xE0o: S\u1EED d\u1EE5ng m\xF4-\u0111un `sys` c\u1EE7\
  a Python, b\u1EA1n c\xF3 th\u1EC3 b\u1EAFt l\u1EA5y c\xE1c \u0111\u1ED1i s\u1ED1\
  \ d\xF2ng l\u1EC7nh m\u1ED9t c\xE1ch d\u1EC5 d\xE0ng. \u0110\xE2y l\xE0 c\xE1ch\
  \ \u0111\u1EC3 truy c\u1EADp ch\xFAng trong m\xE3 c\u1EE7a b\u1EA1n."
lastmod: '2024-03-13T22:44:36.117841-06:00'
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng m\xF4-\u0111un `sys` c\u1EE7a Python, b\u1EA1n c\xF3 th\u1EC3\
  \ b\u1EAFt l\u1EA5y c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh m\u1ED9t c\xE1\
  ch d\u1EC5 d\xE0ng."
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

## Làm thế nào:
Sử dụng mô-đun `sys` của Python, bạn có thể bắt lấy các đối số dòng lệnh một cách dễ dàng. Đây là cách để truy cập chúng trong mã của bạn:

```python
import sys

# Đối số đầu tiên luôn là tên của script, vì vậy chúng ta bỏ qua nó
arguments = sys.argv[1:]

# Làm gì đó với các đối số
print("Bạn đã nhập:", arguments)
```

Chạy mã của bạn như thế này:

```bash
python your_script.py these are your arguments
```

Kết quả mẫu:

```
Bạn đã nhập: ['these', 'are', 'your', 'arguments']
```

## Sâu hơn nữa
Ngày xưa, mọi người tương tác với máy tính qua dòng lệnh. Đó là lý do tại sao hầu hết các ngôn ngữ, bao gồm Python, đều có cách để đọc các đối số dòng lệnh. Đó là cách điều khiển mã trước khi GUI xuất hiện.

`sys.argv` của Python rất tiện ích, nhưng cho phép phân tích lệnh phức tạp hơn, có mô-đun `argparse`. `argparse` là một mô-đun cho khi bạn cần nhiều hơn cơ bản – như khi đối số của bạn cần có tên, kiểu, hoặc giá trị mặc định.

Bây giờ, `sys.argv` chỉ là một danh sách. Mọi thứ bạn truyền đều là chuỗi, bất kể thế nào. Không có phép màu nào cả; nếu bạn muốn số, hãy tự chuyển đổi chúng với cái gì đó như `int()` hoặc `float()`.

## Xem thêm
Để biết thêm về `sys.argv` và `argparse`, hãy xem tài liệu Python:

- `sys.argv`: https://docs.python.org/3/library/sys.html#sys.argv
- Hướng dẫn `argparse`: https://docs.python.org/3/howto/argparse.html 

Và nếu bạn thực sự muốn lao đầu vào giao diện dòng lệnh:

- Click: https://click.palletsprojects.com/en/7.x/
- docopt: http://docopt.org/ 

Chúc bạn lập trình vui vẻ!

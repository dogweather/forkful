---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:37.723176-07:00
description: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh cho ph\xE9\
  p m\xE3 Python c\u1EE7a b\u1EA1n t\u01B0\u01A1ng t\xE1c t\u1ED1t v\u1EDBi c\xE1\
  c \u0111\u1EA7u v\xE0o c\u1EE7a ng\u01B0\u1EDDi d\xF9ng t\u1EEB terminal. T\u1EA1\
  i sao? V\xEC linh ho\u1EA1t l\xE0 ch\xECa kh\xF3a; ng\u01B0\u1EDDi\u2026"
lastmod: '2024-03-11T00:14:09.360842-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh cho ph\xE9\
  p m\xE3 Python c\u1EE7a b\u1EA1n t\u01B0\u01A1ng t\xE1c t\u1ED1t v\u1EDBi c\xE1\
  c \u0111\u1EA7u v\xE0o c\u1EE7a ng\u01B0\u1EDDi d\xF9ng t\u1EEB terminal. T\u1EA1\
  i sao? V\xEC linh ho\u1EA1t l\xE0 ch\xECa kh\xF3a; ng\u01B0\u1EDDi\u2026"
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Đọc các đối số dòng lệnh cho phép mã Python của bạn tương tác tốt với các đầu vào của người dùng từ terminal. Tại sao? Vì linh hoạt là chìa khóa; người dùng có thể điều chỉnh hành vi mà không cần chỉnh sửa mã quý giá của bạn.

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

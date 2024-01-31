---
title:                "Đọc các đối số dòng lệnh"
date:                  2024-01-28T22:05:37.723176-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc các đối số dòng lệnh"

category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

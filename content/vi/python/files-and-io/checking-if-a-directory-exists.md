---
title:                "Kiểm tra xem thư mục có tồn tại không"
aliases:
- /vi/python/checking-if-a-directory-exists.md
date:                  2024-01-28T21:56:55.617265-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kiểm tra xem thư mục có tồn tại không"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Kiểm tra xem một thư mục có tồn tại trong Python hay không là để xác định sự hiện diện của một thư mục trên hệ thống tệp trước khi thực hiện các hành động trên nó. Các lập trình viên làm điều này để tránh những lỗi như cố gắng truy cập hoặc viết vào một thư mục không tồn tại.

## Làm thế nào:

Python làm cho việc kiểm tra một thư mục trở nên trực tiếp với các module `os` và `pathlib`:

Sử dụng `os.path`:
```python
import os

# Kiểm tra xem thư mục có tồn tại
if os.path.isdir("/đường/dẫn/đến/thư/mục"):
    print("Thư mục tồn tại.")
else:
    print("Thư mục không tồn tại.")
```

Sử dụng `pathlib`:
```python
from pathlib import Path

# Kiểm tra xem thư mục có tồn tại
directory = Path("/đường/dẫn/đến/thư/mục")
if directory.is_dir():
    print("Thư mục tồn tại.")
else:
    print("Thư mục không tồn tại.")
```

Đầu ra ví dụ:
```
Thư mục tồn tại.
```
hoặc
```
Thư mục không tồn tại.
```

## Đi sâu:

Trong lịch sử, Python đã sử dụng module `os` cho các hoạt động hệ thống tệp. Tuy nhiên, `os.path.isdir()` đã là tiêu chuẩn de facto để kiểm tra các thư mục. Vấn đề là `os.path` làm việc với chuỗi cho các đường dẫn, có thể là thô kệch.

Nhập vào module `pathlib` hiện đại hơn, được giới thiệu trong Python 3.4. Nó sử dụng các đường dẫn hướng đối tượng, khiến mã trở nên dễ đọc và ngắn gọn hơn. Bây giờ bạn có `Path.is_dir()`, một phương thức không chỉ làm cho mã của bạn gọn gàng hơn, mà còn có điều gì đó dễ chịu khi gọi phương thức liên tiếp tới một đối tượng Path.

Nếu những phương pháp này trả về `False` cho một thư mục không tồn tại, có thể có hai nguyên nhân: hoặc là thư mục thực sự không ở đó, hoặc chương trình của bạn không có quyền để thấy nó.

## Xem thêm:

1. Tài liệu module `os`: https://docs.python.org/3/library/os.html
2. Tài liệu module `pathlib`: https://docs.python.org/3/library/pathlib.html
3. Quyền truy cập hệ thống tệp trong Python: https://docs.python.org/3/library/os.html#os.access

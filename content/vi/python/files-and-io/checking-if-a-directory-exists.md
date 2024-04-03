---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:55.617265-07:00
description: "L\xE0m th\u1EBF n\xE0o: Python l\xE0m cho vi\u1EC7c ki\u1EC3m tra m\u1ED9\
  t th\u01B0 m\u1EE5c tr\u1EDF n\xEAn tr\u1EF1c ti\u1EBFp v\u1EDBi c\xE1c module `os`\
  \ v\xE0 `pathlib`: S\u1EED d\u1EE5ng `os.path`."
lastmod: '2024-03-13T22:44:36.116541-06:00'
model: gpt-4-0125-preview
summary: "Python l\xE0m cho vi\u1EC7c ki\u1EC3m tra m\u1ED9t th\u01B0 m\u1EE5c tr\u1EDF\
  \ n\xEAn tr\u1EF1c ti\u1EBFp v\u1EDBi c\xE1c module `os` v\xE0 `pathlib`."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

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

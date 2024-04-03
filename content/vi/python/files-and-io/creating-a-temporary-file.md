---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:38.428994-07:00
description: "L\xE0m th\u1EBF n\xE0o: M\xF4-\u0111un `tempfile` c\u1EE7a Python \u0111\
  \u01B0\u1EE3c x\xE2y d\u1EF1ng cho m\u1EE5c \u0111\xEDch n\xE0y. H\xE3y xem n\xF3\
  \ ho\u1EA1t \u0111\u1ED9ng nh\u01B0 th\u1EBF n\xE0o."
lastmod: '2024-03-13T22:44:36.122977-06:00'
model: gpt-4-0125-preview
summary: "M\xF4-\u0111un `tempfile` c\u1EE7a Python \u0111\u01B0\u1EE3c x\xE2y d\u1EF1\
  ng cho m\u1EE5c \u0111\xEDch n\xE0y."
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
weight: 21
---

## Làm thế nào:
Mô-đun `tempfile` của Python được xây dựng cho mục đích này. Hãy xem nó hoạt động như thế nào:

```Python
import tempfile

# Tạo một tệp tạm thời và viết gì đó vào trong đó
với tempfile.TemporaryFile(mode='w+t') như tf:
    # Viết một chuỗi vào tệp tạm thời
    tf.write('Python thật vui!')
    # Quay lại đầu tệp trước khi đọc
    tf.seek(0)
    # Đọc những gì chúng ta đã viết
    print(tf.read())  # Kết quả: Python thật vui!

# Và như vậy, tệp sẽ biến mất khi bạn ra khỏi khối
```

Mã này sử dụng một bộ quản lý ngữ cảnh để xử lý tệp, tự động dọn dẹp sau khi sử dụng. Không có tệp lưu lại!

## Sâu hơn:
Tệp tạm không phải là điều mới. Chúng đã được sử dụng từ thuở sơ khai của máy tính để giữ dữ liệu nhất thời. Mô-đun `tempfile` của Python xử lý các chi tiết phức tạp như tạo tên duy nhất và loại bỏ các tệp khi hoàn thành. Nếu bạn muốn kiểm soát nhiều hơn, có `NamedTemporaryFile`, mà bạn có thể tham chiếu bằng tên trong suốt thời gian ngắn ngủi của nó. Nhưng nhớ rằng mục đích của nó là để tạm thời:

```Python
import tempfile

# Tạo một tệp tạm thời có tên
với tempfile.NamedTemporaryFile(delete=True) như ntf:
    print(f'Tên tệp tạm là: {ntf.name}')  # Nó có một tên thực sự!

# Dù vậy, nó biến mất sau khi sử dụng
```

Và tại sao không sử dụng các tệp thông thường? Đơn giản: Sử dụng `tempfile` giúp bạn tránh bị rối và các xung đột tiềm ẩn — tưởng tượng script của bạn chạy lại và cùng một tên tệp được tái sử dụng. Lộn xộn, phải không?

## Xem thêm:
- Tài liệu của Python về tempfile: https://docs.python.org/3/library/tempfile.html
- Hướng dẫn về I/O tệp trong Python: https://realpython.com/read-write-files-python/

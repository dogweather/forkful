---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:38.428994-07:00
description: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi t\u1EE9c l\xE0 b\u1EA1\
  n t\u1EA1o m\u1ED9t t\u1EC7p m\xE0 b\u1EA1n kh\xF4ng c\u1EA7n gi\u1EEF l\u1EA1i\
  \ sau khi s\u1EED d\u1EE5ng xong. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n\
  \ vi\u1EC7c n\xE0y cho d\u1EEF li\u1EC7u ch\u1EC9 c\u1EA7n thi\u1EBFt\u2026"
lastmod: 2024-02-19 22:04:55.318312
model: gpt-4-0125-preview
summary: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi t\u1EE9c l\xE0 b\u1EA1n t\u1EA1\
  o m\u1ED9t t\u1EC7p m\xE0 b\u1EA1n kh\xF4ng c\u1EA7n gi\u1EEF l\u1EA1i sau khi s\u1EED\
  \ d\u1EE5ng xong. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0\
  y cho d\u1EEF li\u1EC7u ch\u1EC9 c\u1EA7n thi\u1EBFt\u2026"
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Tạo một tệp tạm thời tức là bạn tạo một tệp mà bạn không cần giữ lại sau khi sử dụng xong. Lập trình viên thực hiện việc này cho dữ liệu chỉ cần thiết trong quá trình thực thi chương trình, như kết quả trung gian hoặc để tránh chiếm dụng bộ nhớ.

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

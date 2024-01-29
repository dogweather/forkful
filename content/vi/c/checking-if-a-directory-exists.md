---
title:                "Kiểm tra xem thư mục có tồn tại không"
date:                  2024-01-28T21:55:52.875317-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kiểm tra xem thư mục có tồn tại không"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Kiểm tra xem một thư mục có tồn tại hay không là để xác định xem một thư mục cụ thể có mặt trong hệ thống tệp hay không. Lập trình viên thực hiện việc này để tránh các lỗi như cố gắng truy cập hoặc tạo tệp trong một thư mục không tồn tại, có thể làm sập chương trình hoặc dẫn đến mất dữ liệu.

## Cách thực hiện:

Chúng ta sẽ sử dụng hàm `stat` từ tiêu đề `sys/stat.h` để kiểm tra sự tồn tại của thư mục trong C. Dưới đây là một ví dụ mã đơn giản:

```C
#include <stdio.h>
#include <sys/stat.h>

int directory_exists(const char *path) {
    struct stat statbuf;
    if (stat(path, &statbuf) != 0) {
        return 0; // Thư mục không tồn tại hoặc lỗi đã xảy ra
    }
    return S_ISDIR(statbuf.st_mode);
}

int main() {
    const char *path_to_check = "/path/to/directory";
    if (directory_exists(path_to_check)) {
        printf("Thư mục tồn tại!\n");
    } else {
        printf("Thư mục không tồn tại.\n");
    }
    return 0;
}
```

Kết quả mẫu nếu thư mục tồn tại:

```
Thư mục tồn tại!
```

Hoặc, nếu không:

```
Thư mục không tồn tại.
```

## Tìm hiểu sâu hơn

Hàm `stat` đã tồn tại từ những ngày đầu của Unix, là một phần của các tiêu chuẩn POSIX. Nó lấy thông tin về tệp hoặc thư mục tại đường dẫn đã cho, và thông tin đó được lưu trữ trong một `struct stat`. Cụ thể, chúng ta kiểm tra trường `st_mode` để xác định nếu đường dẫn chỉ đến một thư mục.

Các phương án thay thế cho `stat` bao gồm `access` hoặc `fstatat` trong C. Trong Linux, bạn cũng có thể truy cập vào các API cấp cao hơn như `g_file_test` từ thư viện GLib.

Đối với chi tiết triển khai, hãy ghi nhớ những điều này:

- `stat` có thể thất bại không chỉ khi thư mục không tồn tại mà còn do vấn đề về quyền hoặc đường dẫn sai. Kiểm tra lỗi là rất quan trọng.
- Các liên kết biểu tượng cần xử lý đặc biệt; `lstat` được sử dụng thay cho `stat` nếu bạn đang xử lý chúng.
- Hiệu suất có thể biến đổi. Nếu bạn đang kiểm tra nhiều thuộc tính hoặc thực hiện nhiều kiểm tra, có thể có những cách thức hiệu quả hơn.

## Xem thêm

- Tài liệu `stat` của POSIX: [https://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html](https://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html)
- Công cụ Tệp GLib: [https://docs.gtk.org/glib/func.file_test.html](https://docs.gtk.org/glib/func.file_test.html)

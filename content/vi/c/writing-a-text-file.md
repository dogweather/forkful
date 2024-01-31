---
title:                "Viết một tệp văn bản"
date:                  2024-01-28T22:12:52.452140-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc ghi một tệp văn bản bao gồm việc lưu trữ dữ liệu dưới dạng định dạng có thể đọc được trên hệ thống tệp. Lập trình viên thực hiện việc này để lưu giữ thông tin như cài đặt, bản ghi, hoặc nội dung do người dùng tạo ra, cho việc truy xuất và xử lý sau này.

## Làm thế nào:
Ghi vào một tệp văn bản trong C rất đơn giản với các hàm của thư viện `stdio.h`: `fopen()`, `fprintf()`, và `fclose()`. Hãy xem một ví dụ đơn giản:

```C
#include <stdio.h>

int main() {
    FILE *filePointer = fopen("example.txt", "w"); // Mở tệp ở chế độ ghi
    if (filePointer == NULL) {
        printf("Lỗi khi mở tệp.\n");
        return 1;
    }
    
    fprintf(filePointer, "Xin chào, thế giới!\n"); // Ghi vào tệp
    fprintf(filePointer, "Việc ghi vào tệp trong C rất đơn giản.\n");
    
    fclose(filePointer); // Đóng tệp
    return 0;
}
```
Kết quả mẫu trong `example.txt`:
```
Xin chào, thế giới!
Việc ghi vào tệp trong C rất đơn giản.
```

## Đào sâu
Kể từ khi ra đời cùng với các ngôn ngữ tiền thân của C, vào/ra tệp đã luôn là một phần quan trọng của các chương trình. Các lựa chọn thay thế cho `stdio.h` bao gồm các cuộc gọi cấp hệ thống như `open()`, `write()`, và `close()` từ `sys/file.h`, chúng cung cấp nhiều quyền kiểm soát hơn nhưng phức tạp hơn. Khi sử dụng `stdio.h`, việc đệm có thể ảnh hưởng đến hiệu suất, vì vậy đối với các tệp lớn hoặc ghi thường xuyên, hàm `fflush()` có thể cần thiết.

## Xem thêm
Để biết thêm về các hoạt động tệp trong C:
- Tài liệu Thư viện Chuẩn C: https://en.cppreference.com/w/c/io
- Hướng dẫn I/O Tệp trong C: http://www.cplusplus.com/doc/tutorial/files/
- Quản lý I/O Tệp: https://www.gnu.org/software/libc/manual/html_node/File-System-Interface.html

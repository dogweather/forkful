---
title:                "Xử lý lỗi"
date:                  2024-01-28T22:01:53.368151-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xử lý lỗi"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Xử lý lỗi trong C là lường trước những điều không mong đợi. Nó ngăn chương trình đi sai hướng khi gặp phải vấn đề. Lập trình viên làm vậy để xử lý lỗi một cách nhẹ nhàng và giữ cho code của họ đáng tin cậy.

## Làm thế nào:

Hãy xem cách làm điều này trong C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("nonexistentfile.txt", "r");
    if (fp == NULL) {
        perror("Lỗi khi mở file");
        return EXIT_FAILURE;
    }
    // Làm gì đó với file
    fclose(fp);
    return EXIT_SUCCESS;
}
```

Kết quả mẫu khi file không tồn tại:
```
Lỗi khi mở file: Không có file hoặc thư mục như thế
```

## Tìm hiểu sâu hơn

Trong những ngày đầu của C, xử lý lỗi khá đơn giản - chủ yếu là mã trả về và kiểm tra thủ công. Chào mừng `errno`, biến toàn cục được cập nhật khi hàm thất bại. Nó không an toàn với thread chỉ bằng chính nó, do đó, các hàm `strerror` và `perror` mới được giới thiệu để báo cáo lỗi tốt hơn.

Có cách khác? C hiện đại không giới hạn ở `errno`. Có setjmp và longjmp cho các nhảy không-lokal khi xảy ra thảm họa. Một số người thích định nghĩa mã lỗi của riêng họ, trong khi những người khác lựa chọn cấu trúc giống như ngoại lệ trong C++.

Chi tiết triển khai có thể phức tạp. Ví dụ, `errno` an toàn với thread trong hệ thống tuân thủ POSIX nhờ vào phép màu của Bộ Nhớ Địa Phương Thread (TLS). Trong hệ thống nhúng, nơi tài nguyên quý giá, mã xử lý lỗi tùy chỉnh có thể được ưa chuộng hơn các cách tiếp cận tiêu chuẩn có thể làm phình to phần mềm.

## Xem thêm

- Một cái nhìn chi tiết vào `errno`: https://en.cppreference.com/w/c/error/errno
- Về an toàn với thread, xem POSIX threads và errno: http://man7.org/linux/man-pages/man3/pthread_self.3.html
- Giới thiệu về setjmp và longjmp: https://www.cplusplus.com/reference/csetjmp/
- Về xử lý ngoại lệ trong C++, xem: https://isocpp.org/wiki/faq/exceptions

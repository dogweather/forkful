---
title:                "Ghi vào lỗi chuẩn"
date:                  2024-01-28T22:13:19.796125-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc viết vào lỗi chuẩn (stderr) là cách bạn xuất thông điệp lỗi và chẩn đoán trong C. Nó tách biệt với xuất chuẩn (stdout) để bạn có thể xử lý đầu ra thông thường và lỗi một cách khác biệt, như ghi nhận lỗi hoặc tối ưu hóa việc gỡ lỗi.

## Cách thực hiện:

Dưới đây là cách viết vào stderr, sử dụng các hàm của thư viện tiêu chuẩn.

```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Một lỗi đã xảy ra!\n");
    return 0;
}
```

Đầu ra mẫu:

```
Một lỗi đã xảy ra!
```

Sử dụng `perror` khi bạn muốn thêm thông điệp về lỗi hệ thống cuối cùng:

```C
#include <stdio.h>
#include <errno.h>

int main() {
    fopen("nonexistentfile.txt", "r");

    if (errno) {
        perror("Mở file thất bại");
    }

    return 0;
}
```

Đầu ra mẫu:

```
Mở file thất bại: Không có file hoặc thư mục
```

## Đi sâu hơn

Xét về mặt lịch sử, việc tách stderr ra khỏi stdout giúp khi chạy các chương trình từ shell. Xuất chuẩn có thể được điều hướng vào một tập tin hoặc một chương trình khác trong khi lỗi chuẩn vẫn hiển thị trong terminal. Sự phân biệt này rất quan trọng trong các hệ thống dựa trên Unix.

Các phương án thay thế cho `fprintf` hoặc `perror` bao gồm việc viết trực tiếp vào mô tả tập tin, như `write(2, "Lỗi\n", 6);`, tuy nhiên nó ít phổ biến vì đây là hành động cấp thấp hơn.

Về mặt triển khai, stderr là một con trỏ `FILE` được đệm. Nhưng, không giống stdout, nó thường được đặt trong chế độ không đệm để các thông điệp lỗi được hiển thị ngay lập tức, điều này rất quan trọng để hiểu các vấn đề của chương trình khi chúng xuất hiện.

## Xem thêm

- [Thư viện C của GNU: Luồng Chuẩn](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
- [Trang Hướng dẫn Write Syscall](https://man7.org/linux/man-pages/man2/write.2.html)

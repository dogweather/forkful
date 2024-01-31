---
title:                "Xóa các ký tự phù hợp với một mẫu"
date:                  2024-01-28T21:58:46.424090-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Xóa các ký tự khớp với một mẫu trong C là về việc tìm kiếm và loại bỏ các chuỗi ký tự cụ thể từ chuỗi. Các lập trình viên làm điều này để làm sạch đầu vào, thao tác văn bản hoặc chuẩn bị dữ liệu cho việc xử lý.

## Làm thế nào:
Để xóa các ký tự khớp với một mẫu khỏi một chuỗi, chúng ta có thể sử dụng hàm `strpbrk` để tìm các sự xuất hiện và `strcpy` hoặc `memmove` để chuyển văn bản xung quanh. Dưới đây là một ví dụ nhanh:

```c
#include <stdio.h>
#include <string.h>

void delete_pattern(char *str, const char *pattern) {
    char *match;
    while ((match = strpbrk(str, pattern)) != NULL) {
        memmove(match, match + 1, strlen(match));
    }
}

int main() {
    char text[] = "Hello, World! Today is 2023.";
    delete_pattern(text, "o3!");
    printf("%s\n", text); // Kết quả: Hell, Wrld Tday is 22.
    return 0;
}
```
Đoạn mã này tìm kiếm và xóa bỏ các ký tự 'o', '3', và '!' khỏi chuỗi.

## Sâu hơn nữa
Trước đây, trước khi các hàm như `strpbrk` trở thành tiêu chuẩn, các lập trình viên thường viết vòng lặp kiểm tra từng ký tự so với một mẫu—đây là công việc tẻ nhạt nhưng cần thiết. Thư viện tiêu chuẩn C hiện đại đã loại bỏ rất nhiều công việc gian khổ đó, nhưng luôn tốt khi hiểu điều gì diễn ra bên dưới.

`strpbrk` quét một chuỗi để tìm sự khớp đầu tiên trong một nhóm ký tự, và `memmove` an toàn di chuyển byte xung quanh, ngay cả khi chúng gặp nhau. Điều này khác với `strcpy`, không thể xử lý bộ nhớ chồng chéo mà không gặp sự cố.

Các lựa chọn thay thế bao gồm thư viện biểu thức chính quy cho các mẫu phức tạp hoặc vòng lặp thủ công để kiểm soát một cách chính xác. Nhưng luôn là một sự đánh đổi giữa việc bao gồm thư viện ngoài hay tự tạo giải pháp cho hiệu suất hoặc hạn chế bộ nhớ.

## Xem thêm
- [Hàm Chuỗi C](https://www.cplusplus.com/reference/cstring/)
- [Biểu Thức Chính Quy trong C](https://www.regular-expressions.info/posix.html)

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:52:54.196459-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong C, s\u1EF1 t\u1ED3n t\u1EA1i c\u1EE7a\
  \ m\u1ED9t th\u01B0 m\u1EE5c c\xF3 th\u1EC3 \u0111\u01B0\u1EE3c ki\u1EC3m tra s\u1EED\
  \ d\u1EE5ng h\xE0m `stat`, n\xF3 l\u1EA5y th\xF4ng tin v\u1EC1 t\u1EADp tin ho\u1EB7\
  c th\u01B0 m\u1EE5c \u1EDF m\u1ED9t \u0111\u01B0\u1EDDng d\u1EABn c\u1EE5 th\u1EC3\
  .\u2026"
lastmod: '2024-03-13T22:44:37.287541-06:00'
model: gpt-4-0125-preview
summary: "Trong C, s\u1EF1 t\u1ED3n t\u1EA1i c\u1EE7a m\u1ED9t th\u01B0 m\u1EE5c c\xF3\
  \ th\u1EC3 \u0111\u01B0\u1EE3c ki\u1EC3m tra s\u1EED d\u1EE5ng h\xE0m `stat`, n\xF3\
  \ l\u1EA5y th\xF4ng tin v\u1EC1 t\u1EADp tin ho\u1EB7c th\u01B0 m\u1EE5c \u1EDF\
  \ m\u1ED9t \u0111\u01B0\u1EDDng d\u1EABn c\u1EE5 th\u1EC3."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

## Làm thế nào:
Trong C, sự tồn tại của một thư mục có thể được kiểm tra sử dụng hàm `stat`, nó lấy thông tin về tập tin hoặc thư mục ở một đường dẫn cụ thể. Macro `S_ISDIR` từ `sys/stat.h` sau đó được sử dụng để đánh giá liệu thông tin lấy được có tương ứng với một thư mục hay không.

Dưới đây là cách bạn có thể sử dụng `stat` và `S_ISDIR` để kiểm tra xem một thư mục có tồn tại hay không:

```c
#include <stdio.h>
#include <sys/stat.h>

int main() {
    struct stat stats;
    
    // Đường dẫn của thư mục cần kiểm tra
    char *dirPath = "/path/to/directory";

    // Lấy trạng thái của đường dẫn
    int result = stat(dirPath, &stats);

    // Kiểm tra xem thư mục có tồn tại không
    if (result == 0 && S_ISDIR(stats.st_mode)) {
        printf("Thư mục tồn tại.\n");
    } else {
        printf("Thư mục không tồn tại.\n");
    }

    return 0;
}
```

Kết quả mẫu:
```
Thư mục tồn tại.
```

Hoặc, nếu thư mục không tồn tại:
```
Thư mục không tồn tại.
```

## Đi sâu hơn:
Cấu trúc và hàm `stat` đã là một phần của ngôn ngữ lập trình C từ hàng thập kỷ nay, bắt nguồn từ Unix. Chúng cung cấp một cách chuẩn hóa để lấy thông tin hệ thống tập tin, mặc dù là tương đối cấp thấp, nhưng được sử dụng rộng rãi do sự đơn giản và truy cập trực tiếp vào meta dữ liệu của hệ thống tập tin.

Lịch sử, việc kiểm tra sự tồn tại và thuộc tính của các tập tin và thư mục với `stat` và các biến thể của nó (như `fstat` và `lstat`) đã là một cách tiếp cận phổ biến. Tuy nhiên, các hàm này tương tác trực tiếp với nhân hệ điều hành, có thể giới thiệu sự chậm chạp và những lỗi tiềm ẩn nếu không được xử lý đúng cách.

Đối với các dự án mới hoặc khi làm việc trong các tình huống cấp cao, lập trình viên có thể lựa chọn các cơ chế xử lý tập tin trừu tượng hơn được cung cấp bởi các khung công việc hiện đại hoặc thư viện, điều này xử lý lỗi một cách nhã nhặn hơn và cung cấp một API đơn giản hơn. Tuy nhiên, việc hiểu và có khả năng sử dụng `stat` vẫn là một kỹ năng quý giá cho các tình huống yêu cầu thao tác trực tiếp với hệ thống tập tin, như lập trình hệ thống hoặc khi làm việc trong môi trường hạn chế việc phụ thuộc vào các thư viện lớn là không khả thi.

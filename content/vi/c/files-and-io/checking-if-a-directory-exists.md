---
title:                "Kiểm tra xem thư mục có tồn tại không"
aliases:
- /vi/c/checking-if-a-directory-exists.md
date:                  2024-02-03T17:52:54.196459-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kiểm tra xem thư mục có tồn tại không"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Việc kiểm tra xem một thư mục có tồn tại trong C hay không bao gồm việc truy vấn vào hệ thống tập tin để xác minh liệu một đường dẫn cụ thể có dẫn đến một thư mục hay không. Lập trình viên thường thực hiện thao tác này để đảm bảo các hoạt động với tập tin (như đọc từ hoặc ghi vào tập tin) được hướng đến các đường dẫn hợp lệ, tránh lỗi và nâng cao độ tin cậy của phần mềm.

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

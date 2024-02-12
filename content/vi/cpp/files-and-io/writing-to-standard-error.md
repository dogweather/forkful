---
title:                "Ghi vào lỗi chuẩn"
aliases:
- /vi/cpp/writing-to-standard-error.md
date:                  2024-01-28T22:13:56.926144-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
Việc ghi vào lỗi chuẩn (`stderr`) có nghĩa là gửi các thông báo lỗi và thông tin chẩn đoán đến một luồng riêng biệt, tách biệt khỏi đầu ra thường (`stdout`). Các lập trình viên làm điều này để tách biệt rõ ràng đầu ra bình thường khỏi các lỗi, làm cho việc xử lý và gỡ lỗi chương trình dễ dàng hơn.

## Làm thế nào:
C++ sử dụng `cerr` để ghi vào `stderr`. Dưới đây là cách sử dụng:

```cpp
#include <iostream>

int main() {
    std::cout << "Đây là đầu ra thường" << std::endl;
    std::cerr << "Đây là thông báo lỗi" << std::endl;
    return 0;
}
```

Đầu ra mẫu có thể trông như thế này:

```
Đây là đầu ra thường
Đây là thông báo lỗi
```

Ngay cả khi bạn chuyển hướng `stdout`, `stderr` vẫn hiển thị trong terminal:

```cpp
// Chuyển hướng stdout sang một tệp, nhưng stderr vẫn hiển thị trong terminal
int main() {
    freopen("output.txt", "w", stdout);
    std::cout << "Điều này không hiển thị trên terminal" << std::endl;
    std::cerr << "Điều này sẽ hiển thị trong terminal" << std::endl;
    fclose(stdout);
    return 0;
}
```

## Sâu hơn:
Trong các hệ thống giống UNIX, `stderr` được giới thiệu để tách đầu ra của chương trình (`stdout`) khỏi các thông báo lỗi, với mỗi thứ có mã số định danh tệp riêng biệt của mình (1 cho `stdout`, 2 cho `stderr`). Các phương pháp thay thế cho `cerr` là sử dụng `fprintf(stderr, ...)` trong C hoặc ghi trực tiếp vào mã số định danh tệp 2. Bên trong, `cerr` là một thể hiện của `ostream` và không được đệm để đảm bảo đầu ra lỗi ngay lập tức mà không chờ đợi bộ đệm đầy.

## Xem thêm:
- [cppreference std::cerr](https://en.cppreference.com/w/cpp/io/cerr)
- [Thư viện C của GNU: Luồng Chuẩn](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
- [Chuyển hướng stdout và stderr](http://www.cplusplus.com/reference/cstdio/freopen/)

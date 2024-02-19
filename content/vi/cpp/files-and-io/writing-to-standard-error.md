---
aliases:
- /vi/cpp/writing-to-standard-error/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:56.926144-07:00
description: "Vi\u1EC7c ghi v\xE0o l\u1ED7i chu\u1EA9n (`stderr`) c\xF3 ngh\u0129\
  a l\xE0 g\u1EEDi c\xE1c th\xF4ng b\xE1o l\u1ED7i v\xE0 th\xF4ng tin ch\u1EA9n \u0111\
  o\xE1n \u0111\u1EBFn m\u1ED9t lu\u1ED3ng ri\xEAng bi\u1EC7t, t\xE1ch bi\u1EC7t kh\u1ECF\
  i \u0111\u1EA7u ra th\u01B0\u1EDDng\u2026"
lastmod: 2024-02-18 23:08:51.062038
model: gpt-4-0125-preview
summary: "Vi\u1EC7c ghi v\xE0o l\u1ED7i chu\u1EA9n (`stderr`) c\xF3 ngh\u0129a l\xE0\
  \ g\u1EEDi c\xE1c th\xF4ng b\xE1o l\u1ED7i v\xE0 th\xF4ng tin ch\u1EA9n \u0111o\xE1\
  n \u0111\u1EBFn m\u1ED9t lu\u1ED3ng ri\xEAng bi\u1EC7t, t\xE1ch bi\u1EC7t kh\u1ECF\
  i \u0111\u1EA7u ra th\u01B0\u1EDDng\u2026"
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:56.926144-07:00
description: "L\xE0m th\u1EBF n\xE0o: C++ s\u1EED d\u1EE5ng `cerr` \u0111\u1EC3 ghi\
  \ v\xE0o `stderr`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch s\u1EED d\u1EE5ng."
lastmod: '2024-03-13T22:44:37.063622-06:00'
model: gpt-4-0125-preview
summary: "C++ s\u1EED d\u1EE5ng `cerr` \u0111\u1EC3 ghi v\xE0o `stderr`."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

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

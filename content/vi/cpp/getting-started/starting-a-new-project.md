---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:31.595035-07:00
description: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi c\xF3 ngh\u0129\
  a l\xE0 thi\u1EBFt l\u1EADp n\u1EC1n m\xF3ng cho c\u01A1 s\u1EDF m\xE3 c\u1EE7a\
  \ b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ kh\u1EDFi \u0111\u1EA7u qu\xE1 tr\xECnh ph\xE1t tri\u1EC3n, h\u01B0\u1EDBng d\u1EAB\
  n c\u1EA5u\u2026"
lastmod: '2024-03-13T22:44:37.043440-06:00'
model: gpt-4-0125-preview
summary: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi c\xF3 ngh\u0129a\
  \ l\xE0 thi\u1EBFt l\u1EADp n\u1EC1n m\xF3ng cho c\u01A1 s\u1EDF m\xE3 c\u1EE7a\
  \ b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ kh\u1EDFi \u0111\u1EA7u qu\xE1 tr\xECnh ph\xE1t tri\u1EC3n, h\u01B0\u1EDBng d\u1EAB\
  n c\u1EA5u\u2026"
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Bắt đầu một dự án mới có nghĩa là thiết lập nền móng cho cơ sở mã của bạn. Lập trình viên làm điều này để khởi đầu quá trình phát triển, hướng dẫn cấu trúc dự án, và đặt nền tảng cho mã trong tương lai.

## Làm thế nào:
Khi bắt đầu, chọn hệ thống xây dựng hoặc IDE của bạn. Để đơn giản, chúng tôi sẽ sử dụng trình soạn thảo văn bản cơ bản và g++. Tạo hai tệp: `main.cpp` và một `Makefile`.

`main.cpp`:
```C++
#include <iostream>

int main() {
    std::cout << "Xin chào, dự án mới!" << std::endl;
    return 0;
}
```

`Makefile`:
```make
all:
    g++ main.cpp -o my_project

clean:
    rm my_project
```

Để biên dịch, chạy `make` trong terminal. Để dọn dẹp, chạy `make clean`.

Kết quả mẫu sau khi chạy `./my_project`:
```
Xin chào, dự án mới!
```

## Sâu hơn
Trong quá khứ, thiết lập một dự án C++ mới là một quy trình thủ công hơn. Ngày nay, IDE có thể tạo ra các mẫu. Các sự lựa chọn như CMake hoặc Meson giúp quản lý việc xây dựng. Trước khi có những công cụ này, các nhà phát triển tự viết Makefiles bằng tay, biên dịch mỗi tệp `.cpp` thành một tệp đối tượng trước khi liên kết chúng lại với nhau.

Xem xét các lựa chọn thay thế: các hệ thống xây dựng mới hơn đơn giản hóa quá trình. Ví dụ, CMake tự động tạo Makefiles của bạn, làm cho nó độc lập với nền tảng.

Về mặt triển khai, cài đặt phụ thuộc vào các yếu tố như kích thước dự án và các phụ thuộc. Dự án lớn hơn đòi hỏi một cấu trúc phức tạp hơn với các thư mục riêng biệt cho tệp nguồn, tiêu đề, và kiểm tra.

## Xem thêm
- [Tài liệu CMake](https://cmake.org/documentation/)
- [Hướng dẫn cốt lõi C++](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines)
- [GCC, Bộ sưu tập Biên dịch GNU](https://gcc.gnu.org/)

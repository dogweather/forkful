---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:32.479083-07:00
description: "L\xE0m th\u1EBF n\xE0o: C++ cung c\u1EA5p m\u1ED9t c\xE1ch tr\u1EF1\
  c ti\u1EBFp \u0111\u1EC3 t\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i s\u1EED\
  \ d\u1EE5ng ph\u01B0\u01A1ng th\u1EE9c `length()` c\u1EE7a l\u1EDBp `std::string`.\
  \ Nh\u01B0ng n\u1EBFu b\u1EA1n l\xE0 ng\u01B0\u1EDDi\u2026"
lastmod: '2024-03-13T22:44:37.030854-06:00'
model: gpt-4-0125-preview
summary: "C++ cung c\u1EA5p m\u1ED9t c\xE1ch tr\u1EF1c ti\u1EBFp \u0111\u1EC3 t\xEC\
  m chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i s\u1EED d\u1EE5ng ph\u01B0\u01A1\
  ng th\u1EE9c `length()` c\u1EE7a l\u1EDBp `std::string`."
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

## Làm thế nào:
C++ cung cấp một cách trực tiếp để tìm chiều dài của một chuỗi sử dụng phương thức `length()` của lớp `std::string`. Nhưng nếu bạn là người của phong cách cũ, bạn vẫn có thể đi với chuỗi kiểu C và `strlen()`. Dưới đây là cả hai cách thức:

```C++
#include <iostream>
#include <string>
#include <cstring>

int main() {
    // Sử dụng std::string
    std::string greeting = "Hello, World!";
    std::cout << "Chiều dài chuỗi (std::string): " << greeting.length() << std::endl;

    // Sử dụng chuỗi kiểu C
    const char *c_greeting = "Hello, World!";
    std::cout << "Chiều dài chuỗi (kiểu C): " << strlen(c_greeting) << std::endl;

    return 0;
}
```

Kết quả mẫu:
```
Chiều dài chuỗi (std::string): 13
Chiều dài chuỗi (kiểu C): 13
```

## Đi sâu:
Ban đầu, C++ kế thừa mảng ký tự kiểu C và hàm `strlen()` đi kèm từ C. `strlen()` tính chiều dài bằng cách di chuyển qua mảng cho đến khi nó gặp ký tự null, `'\0'`. Đây là một chiến lược đơn giản nhưng hiệu quả nhưng không thể bằng hiệu quả của `std::string.length()`, thường giữ theo dõi chiều dài cho việc truy xuất nhanh chóng.

Có lựa chọn khác? Chắc chắn rồi:
- Bạn cũng có thể sử dụng phương thức `size()`, giống hệt `length()` cho `std::string`.
- Đối với chuỗi ký tự rộng, `std::wstring` và phương thức `length()` của nó là bạn bè của bạn.
- Lựa chọn hấp dẫn hơn bao gồm các hàm tự định nghĩa hoặc sử dụng các thuật toán như `std::distance` với các iterators.

Tuy nhiên, cẩn thận, `std::string::length()` trả về một kiểu `size_t`, một số nguyên không dấu, có thể khiến bạn gặp phải những hành vi không mong muốn nếu bạn trộn lẫn nó với các kiểu dấu trong biểu thức.

## Xem thêm:
- Tham khảo C++ cho `std::string::length()`: https://en.cppreference.com/w/cpp/string/basic_string/length
- Tham khảo C++ cho `strlen()`: https://en.cppreference.com/w/cpp/string/byte/strlen
- Thêm về `std::string` so với chuỗi kiểu C: https://www.learncpp.com/cpp-tutorial/4-4a-c-style-strings/
- Dành cho những người muốn tìm hiểu sâu hơn về lớp `std::string`: https://en.cppreference.com/w/cpp/string/basic_string

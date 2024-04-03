---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:38.128603-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong C++, ch\xFAng ta c\xF3 m\u1ED9t v\xE0\
  i c\xE1ch \u0111\u1EC3 n\u1ED1i c\xE1c chu\u1ED7i. D\u01B0\u1EDBi \u0111\xE2y l\xE0\
  \ m\u1ED9t v\xED d\u1EE5 s\u1EED d\u1EE5ng `std::string` v\xE0 to\xE1n t\u1EED c\u1ED9\
  ng (`+`)."
lastmod: '2024-03-13T22:44:37.032151-06:00'
model: gpt-4-0125-preview
summary: "Trong C++, ch\xFAng ta c\xF3 m\u1ED9t v\xE0i c\xE1ch \u0111\u1EC3 n\u1ED1\
  i c\xE1c chu\u1ED7i."
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

## Làm thế nào:
Trong C++, chúng ta có một vài cách để nối các chuỗi. Dưới đây là một ví dụ sử dụng `std::string` và toán tử cộng (`+`):

```cpp
#include <iostream>
#include <string>

int main() {
    std::string hello = "Hello, ";
    std::string world = "World!";
    
    std::string greeting = hello + world;
    
    std::cout << greeting << std::endl; // Kết quả: Hello, World!
    return 0;
}
```

Nhanh và đơn giản, phải không? Nhưng, chúng ta cũng có thể sử dụng `append()`:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string hello = "Hello, ";
    hello.append("World!");
    
    std::cout << hello << std::endl; // Kết quả: Hello, World!
    return 0;
}
```

Hoặc thậm chí là toán tử `operator+=` nếu bạn muốn:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string hello = "Hello, ";
    hello += "World!";
    
    std::cout << hello << std::endl; // Kết quả: Hello, World!
    return 0;
}
```

## Sâu hơn nữa
Lịch sử, C++ đã tiếp bước từ C, sử dụng các mảng ký tự và chức năng như `strcat()` để làm việc với chuỗi. Điều này rối rắm và dễ gặp lỗi hơn.

C++ hiện đại đã cải thiện tình hình với `std::string`. Nó an toàn hơn, dễ đọc hơn, và mang lại cho bạn nhiều lựa chọn. Nếu `std::string` không phải là gu của bạn, có `std::stringstream` hoặc thậm chí là `std::format` (từ C++20) dành cho những người hâm mộ định dạng.

Về bản chất, việc nối chuỗi bao gồm cấp phát bộ nhớ và sao chép. Nếu thực hiện không cẩn thận, nó có thể ảnh hưởng đến hiệu suất của chương trình như một cú đập. Các con trỏ thông minh và ngữ nghĩa di chuyển giảm bớt một số nỗi đau ở đây.

Đừng quên về các lựa chọn khác - các thư viện như Boost, hoặc xử lý UTF-8 với `std::string_view` cho các hoạt động không sao chép trên C++ hiện đại.

## Xem thêm
- Tài liệu tham khảo C++ cho `std::string`: https://cplusplus.com/reference/string/string/
- Bản thảo làm việc của C++, Tiêu chuẩn cho Ngôn ngữ Lập trình C++: http://www.open-std.org/JTC1/SC22/WG21/docs/papers/2020/n4861.pdf
- Tìm hiểu thêm về `std::format`: https://en.cppreference.com/w/cpp/utility/format
- Tài liệu thư viện Boost: https://www.boost.org/doc/libs/1_75_0/libs/string_algo/doc/html/index.html

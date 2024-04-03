---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:39.157407-07:00
description: "L\xE0m th\u1EBF n\xE0o: C++ kh\xF4ng c\xF3 t\xEDnh n\u0103ng n\u1ED9\
  i suy chu\u1ED7i \u0111\u01B0\u1EE3c t\xEDch h\u1EE3p s\u1EB5n nh\u01B0 m\u1ED9\
  t s\u1ED1 ng\xF4n ng\u1EEF kh\xE1c. B\u1EA1n th\u01B0\u1EDDng s\u1EED d\u1EE5ng\
  \ `std::ostringstream`, `std::format` (t\u1EEB\u2026"
lastmod: '2024-03-13T22:44:37.024318-06:00'
model: gpt-4-0125-preview
summary: "C++ kh\xF4ng c\xF3 t\xEDnh n\u0103ng n\u1ED9i suy chu\u1ED7i \u0111\u01B0\
  \u1EE3c t\xEDch h\u1EE3p s\u1EB5n nh\u01B0 m\u1ED9t s\u1ED1 ng\xF4n ng\u1EEF kh\xE1\
  c."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

## Làm thế nào:
C++ không có tính năng nội suy chuỗi được tích hợp sẵn như một số ngôn ngữ khác. Bạn thường sử dụng `std::ostringstream`, `std::format` (từ C++20), hoặc định dạng kiểu printf.

Với `std::ostringstream`:
```cpp
#include <sstream>
#include <iostream>

int main() {
    std::ostringstream message;
    int age = 30;
    message << "Hello, I am " << age << " năm tuổi.";
    std::cout << message.str() << std::endl; // "Hello, I am 30 năm tuổi."
}
```

Với `std::format` (C++20):
```cpp
#include <format>
#include <iostream>

int main() {
    int age = 30;
    std::string message = std::format("Xin chào, tôi {} năm tuổi.", age);
    std::cout << message << std::endl; // "Xin chào, tôi 30 năm tuổi."
}
```

## Sâu hơn
Trước C++20, chúng ta nối chuỗi bằng cách sử dụng luồng hoặc sprintf, đó là cách không mấy thuận tiện. Với sự xuất hiện của `std::format`, chúng ta đang bắt kịp với các ngôn ngữ hiện đại như Python với f-strings của họ.

`std::ostringstream`: Điều này cung cấp cho chúng ta một cách giống như luồng để xây dựng chuỗi. Nó linh hoạt nhưng không phải là cách gọn nhẹ nhất. Nó đã là lựa chọn hàng đầu trong nhiều năm vì nó an toàn và dễ sử dụng.

`std::format`: Được giới thiệu trong C++20, nó cung cấp định dạng giống như Python. Nó dễ đọc và hiệu quả hơn so với việc nối chuỗi bằng luồng nhưng đòi hỏi trình biên dịch mới hơn.

Có những sự thay thế tồn tại như Boost.Format hoặc sử dụng nối chuỗi, nhưng chúng không được sạch sẽ hoặc có thể gây ra chi phí phụ.

Nội suy chuỗi chỉ là đường nét, nhưng nó ngọt ngào. Điều này đơn giản hóa mã nguồn và tránh được việc giảm hiệu suất do việc liên tục thêm chuỗi.

## Xem thêm
- [cppreference về std::format](https://en.cppreference.com/w/cpp/utility/format)
- [cppreference về std::ostringstream](https://en.cppreference.com/w/cpp/io/basic_ostringstream)
- [Thư viện Boost.Format](https://www.boost.org/doc/libs/release/libs/format/)

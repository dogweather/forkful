---
title:                "Nội suy chuỗi ký tự"
aliases:
- /vi/cpp/interpolating-a-string.md
date:                  2024-01-28T22:02:39.157407-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Là gì & Tại sao?
Nội suy chuỗi là việc chèn các biến vào trong chuỗi. Chúng ta làm điều đó để xây dựng thông điệp một cách linh hoạt, cá nhân hóa đầu ra, hoặc tạo lập truy vấn động.

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

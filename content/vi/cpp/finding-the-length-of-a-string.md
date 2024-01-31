---
title:                "Tìm chiều dài của một chuỗi ký tự"
date:                  2024-01-28T22:00:32.479083-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"

category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Tìm chiều dài của một chuỗi nghĩa là xác định xem nó chứa bao nhiêu ký tự. Lập trình viên thực hiện việc này để xác thực input, thiết lập vòng lặp, hoặc chuẩn bị dữ liệu cho các function API nhất định yêu cầu kích thước chuỗi.

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

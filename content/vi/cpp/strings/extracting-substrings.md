---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:56.998837-07:00
description: "L\xE0m th\u1EBF n\xE0o: C++ l\xE0m cho vi\u1EC7c l\u1EA5y m\u1ED9t chu\u1ED7\
  i con tr\u1EDF n\xEAn d\u1EC5 d\xE0ng. `std::string` l\xE0 \u0111\u1ED3ng minh \u0111\
  \xE1ng tin c\u1EADy c\u1EE7a ch\xFAng ta \u1EDF \u0111\xE2y, v\u1EDBi h\xE0m `substr()`\
  \ th\u1EF1c hi\u1EC7n\u2026"
lastmod: '2024-03-13T22:44:37.028291-06:00'
model: gpt-4-0125-preview
summary: "C++ l\xE0m cho vi\u1EC7c l\u1EA5y m\u1ED9t chu\u1ED7i con tr\u1EDF n\xEA\
  n d\u1EC5 d\xE0ng."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Làm thế nào:
C++ làm cho việc lấy một chuỗi con trở nên dễ dàng. `std::string` là đồng minh đáng tin cậy của chúng ta ở đây, với hàm `substr()` thực hiện phần lớn công việc nặng nhọc. Chúng ta hãy đi thẳng vào một số mã:

```C++
#include <iostream>
#include <string>

int main() {
    std::string fullString = "Hello, World! Programming in C++ is fun.";
    std::string snippet;

    // Trích xuất "World" bắt đầu tại chỉ mục 7 với chiều dài 5
    snippet = fullString.substr(7, 5);
    std::cout << snippet << std::endl; // Đầu ra: World

    // Trích xuất "Programming" bắt đầu tại chỉ mục 14
    snippet = fullString.substr(14);
    std::cout << snippet << std::endl; // Đầu ra: Programming in C++ is fun.

    return 0;
}
```

## Sâu hơn nữa
Chuỗi con không phải là điều mới. Các lập trình viên C cũ đã sử dụng `strncpy` và ghi chép thủ công. Việc xử lý chuỗi là nguồn gốc của nhiều loại lỗi, do đó C++ đã hướng tới việc đơn giản hóa nó. `std::string` và phương pháp `substr` của nó có từ C++98 và đã giảm bớt căng thẳng từ đó.

Có phương án khác? Chắc chắn. Bạn có thể làm thủ công với `std::string::iterator` hoặc lấp bụi cho các hàm C—nếu bạn thích sống mạo hiểm. Một cách tiếp cận hiện đại hơn có thể liên quan đến string_views cho việc liếc nhìn không chỉnh sửa.

Triển khai? Bên trong, `substr` thường phải cấp phát không gian lưu trữ mới và sao chép dữ liệu, điều này không phải là miễn phí. Nó nhẹ nhàng hơn so với việc vật lộn với con trỏ thô và mảng ký tự của thời xa xưa, nhưng nó không tức thì.

## Xem thêm
Để biết thêm về `std::string` và các bạn của nó:
- cppreference.com về `std::string`: https://en.cppreference.com/w/cpp/string/basic_string
- Thêm về `std::string_view`: https://en.cppreference.com/w/cpp/string/basic_string_view
- Xử lý chuỗi kiểu C (cho niềm vui lịch sử): http://www.cplusplus.com/reference/cstring/

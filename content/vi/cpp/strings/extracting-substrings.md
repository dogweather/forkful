---
title:                "Trích xuất chuỗi con"
aliases:
- /vi/cpp/extracting-substrings/
date:                  2024-01-28T21:59:56.998837-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trích xuất chuỗi con"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Trích xuất các chuỗi con có nghĩa là lấy ra những mảnh nhỏ từ một chuỗi lớn hơn. Lập trình viên làm điều này để tách biệt, xử lý hoặc phân tích dữ liệu cụ thể trong văn bản, như lấy tên người dùng từ địa chỉ email hoặc ngày tháng từ nhật ký.

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

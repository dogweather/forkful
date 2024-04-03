---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:45.264475-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: C++ cung c\u1EA5p nhi\u1EC1u c\xE1ch \u0111\
  \u1EC3 bi\u1EBFn ch\u1EEF hoa chu\u1ED7i, nh\u01B0ng \u0111\xE2y l\xE0 m\u1ED9t\
  \ v\xED d\u1EE5 \u0111\u01A1n gi\u1EA3n."
lastmod: '2024-03-13T22:44:37.020489-06:00'
model: gpt-4-0125-preview
summary: "C++ cung c\u1EA5p nhi\u1EC1u c\xE1ch \u0111\u1EC3 bi\u1EBFn ch\u1EEF hoa\
  \ chu\u1ED7i, nh\u01B0ng \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 \u0111\u01A1n\
  \ gi\u1EA3n."
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
weight: 2
---

## Cách thực hiện:
C++ cung cấp nhiều cách để biến chữ hoa chuỗi, nhưng đây là một ví dụ đơn giản:

```cpp
#include <iostream>
#include <algorithm>
#include <string>

std::string capitalizeString(std::string str) {
    std::transform(str.begin(), str.end(), str.begin(), ::toupper);
    return str;
}

int main() {
    std::string text = "Hello, World!";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl;
    return 0;
}
```

Kết quả mẫu:
```
HELLO, WORLD!
```

## Tìm hiểu sâu hơn
Để chuyển chuỗi thành chữ hoa trong C++, truyền thống chúng ta dựa vào việc sử dụng vòng lặp để lần lượt đi qua từng ký tự, áp dụng hàm `toupper` từ `<cctype>`.

Khi C++ phát triển, Thư viện Mẫu Chuẩn (STL) đã cung cấp các thuật toán như `std::transform` có thể áp dụng một hàm lên một dãy. Phong cách này thúc đẩy việc viết code sạch hơn và có khả năng hiệu suất tốt hơn do các tối ưu hóa thuật toán.

Ngoài `std::transform`, còn có lựa chọn sử dụng ranges (từ C++20) giúp cho code còn gọn gàng và biểu cảm hơn nữa. Nhưng đó là một chủ đề cho một ngày khác.

Các phương án thay thế để làm chữ hoa chuỗi bao gồm việc viết hàm của riêng bạn hoặc sử dụng thư viện bên ngoài như Boost. Điều này thực sự phụ thuộc vào mức độ kiểm soát bạn cần và các phụ thuộc bạn sẵn lòng chấp nhận.

Khi sử dụng `std::transform`, hãy lưu ý rằng nó sửa đổi trực tiếp chuỗi. Nếu việc duy trì dạng chữ của chuỗi gốc quan trọng, hãy luôn làm việc trên một bản sao.

## Xem Thêm
- Tài liệu tham khảo C++ cho `std::transform`: https://en.cppreference.com/w/cpp/algorithm/transform
- Tài liệu tham khảo C++ cho `toupper`: https://en.cppreference.com/w/cpp/string/byte/toupper
- Tổng quan về Ranges của C++20: https://en.cppreference.com/w/cpp/ranges

---
title:                "Sử dụng biểu thức chính quy"
date:                  2024-01-28T22:09:47.728338-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"

category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Biểu thức chính quy là các mẫu được sử dụng để khớp các tổ hợp ký tự trong văn bản. Lập trình viên sử dụng chúng cho các tác vụ như xác thực, tìm kiếm và thao tác văn bản do sự mạnh mẽ và linh hoạt của chúng.

## Làm thế nào:

Để sử dụng biểu thức chính quy trong C++, bạn cần bao gồm thư viện `<regex>`. Dưới đây là cách bạn khớp, tìm kiếm và thay thế văn bản:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string target("Hello World. This is a regex test.");
    
    // Khớp
    std::regex match_pattern("Hello World");
    bool is_match = std::regex_match(target, match_pattern);
    std::cout << (is_match ? "Khớp" : "Không khớp") << "\n";
    
    // Tìm kiếm
    std::regex search_pattern("\\bis\\b");
    std::smatch matches;
    if (std::regex_search(target, matches, search_pattern)) {
        std::cout << "Tìm thấy: " << matches[0] << "\n";
    }

    // Thay thế
    std::regex replace_pattern("World");
    std::string result = std::regex_replace(target, replace_pattern, "Universe");
    std::cout << "Sau khi thay thế: " << result << "\n";
    
    return 0;
}
```

Kết quả mẫu:

```
Khớp
Tìm thấy: is
Sau khi thay thế: Hello Universe. This is a regex test.
```

## Sâu hơn

Biểu thức chính quy đã là một phần của khoa học máy tính từ những năm 1950, được phổ biến bởi các tiện ích như grep trong Unix. C++ đã tiếp nhận chúng muộn hơn, với std::regex trong C++11. Sự hỗ trợ bản địa thay đổi theo trình biên dịch; một số có thể lạc hậu trong hỗ trợ đầy đủ tính năng regex.

Các lựa chọn thay thế cho `std::regex` bao gồm các thư viện như Boost.Regex hay PCRE (Perl Compatible Regular Expressions). Boost.Regex, ví dụ, thường vượt trội hơn `std::regex` và có một bộ tính năng phong phú hơn.

Về mặt triển khai, `std::regex` có thể chậm hơn một số thuật toán phân tích cú pháp tùy chỉnh, đặc biệt là cho các mẫu đơn giản. Hiểu biết sự cân nhắc giữa sự tiện lợi của regex và các vấn đề về hiệu suất tiềm ẩn là chìa khóa.

## Xem thêm

- Tài liệu tham khảo C++ về `<regex>`: https://en.cppreference.com/w/cpp/regex
- Tài liệu Boost.Regex: https://www.boost.org/doc/libs/release/libs/regex/
- Trang chính thức của PCRE: https://www.pcre.org/

Đọc thêm và các công cụ để cải thiện kỹ năng regex của bạn:

- Hướng dẫn Regular-Expressions.info: https://www.regular-expressions.info/tutorial.html
- Regex101 (trình kiểm tra trực tuyến): https://regex101.com/

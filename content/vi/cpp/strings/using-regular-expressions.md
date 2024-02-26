---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:47.728338-07:00
description: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy l\xE0 c\xE1c m\u1EABu \u0111\u01B0\u1EE3\
  c s\u1EED d\u1EE5ng \u0111\u1EC3 kh\u1EDBp c\xE1c t\u1ED5 h\u1EE3p k\xFD t\u1EF1\
  \ trong v\u0103n b\u1EA3n. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFAng\
  \ cho c\xE1c t\xE1c v\u1EE5 nh\u01B0 x\xE1c th\u1EF1c, t\xECm ki\u1EBFm v\xE0\u2026"
lastmod: '2024-02-25T18:49:35.376586-07:00'
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy l\xE0 c\xE1c m\u1EABu \u0111\u01B0\u1EE3\
  c s\u1EED d\u1EE5ng \u0111\u1EC3 kh\u1EDBp c\xE1c t\u1ED5 h\u1EE3p k\xFD t\u1EF1\
  \ trong v\u0103n b\u1EA3n. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFAng\
  \ cho c\xE1c t\xE1c v\u1EE5 nh\u01B0 x\xE1c th\u1EF1c, t\xECm ki\u1EBFm v\xE0\u2026"
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
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

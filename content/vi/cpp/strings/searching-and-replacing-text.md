---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:13.493417-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: C++ cung c\u1EA5p nhi\u1EC1u c\xE1ch \u0111\
  \u1EC3 t\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n. D\u01B0\u1EDBi \u0111\
  \xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 s\u1EED d\u1EE5ng `std::string::find` v\xE0 `std::string::replace`."
lastmod: '2024-03-13T22:44:37.023051-06:00'
model: gpt-4-0125-preview
summary: "C++ cung c\u1EA5p nhi\u1EC1u c\xE1ch \u0111\u1EC3 t\xECm ki\u1EBFm v\xE0\
  \ thay th\u1EBF v\u0103n b\u1EA3n."
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
weight: 10
---

## Cách thực hiện:
C++ cung cấp nhiều cách để tìm kiếm và thay thế văn bản. Dưới đây là một ví dụ sử dụng `std::string::find` và `std::string::replace`.

```cpp
#include <iostream>
#include <string>

int main() {
    std::string myText = "The quick brown fox jumps over the lazy dog.";
    std::string wordToSearch = "lazy";
    std::string replacement = "energetic";

    size_t pos = myText.find(wordToSearch);
    
    if (pos != std::string::npos) {
        myText.replace(pos, wordToSearch.length(), replacement);
    }

    std::cout << myText << std::endl; // Output: The quick brown fox jumps over the energetic dog.
    return 0;
}
```

## Sâu hơn nữa
Các hàm `find` và `replace` đã là một phần của lớp `std::string` của C++ từ lâu, làm cho chúng trở thành một phương tiện cơ bản nhưng mạnh mẽ để thao tác văn bản. Trước `std::string`, các lập trình viên C sử dụng mảng ký tự và các hàm như `strstr` và `strcpy` từ Thư viện chuẩn C cho các nhiệm vụ tương tự, điều này dễ gây ra lỗi hơn và yêu cầu quản lý bộ nhớ thủ công.

Đối với các phương án thay thế, các thành phần khác của thư viện chuẩn như `std::regex` cung cấp khả năng thao tác văn bản dựa trên mẫu cho các kịch bản tìm kiếm và thay thế phức tạp. Thư viện của bên thứ ba như Boost thì cung cấp còn nhiều tùy chọn xử lý văn bản tinh vi hơn.

Bên dưới bề mặt, việc tìm kiếm và thay thế bao gồm các thuật toán lặp qua một chuỗi để tìm kiếm các chuỗi ký tự phù hợp và sau đó sửa đổi nội dung của chuỗi một cách phù hợp. Hiệu suất của các thao tác này có thể thay đổi tùy theo cách thực hiện và độ phức tạp của mẫu tìm kiếm.

## Xem thêm
- Tham khảo C++ cho `std::string::find`: https://en.cppreference.com/w/cpp/string/basic_string/find
- Tham khảo C++ cho `std::string::replace`: https://en.cppreference.com/w/cpp/string/basic_string/replace
- Tham khảo C++ cho Biểu thức chính quy `std::regex`: https://en.cppreference.com/w/cpp/regex
- Thư viện Thuật toán Chuỗi của Boost: https://www.boost.org/doc/libs/release/libs/algorithm/string/

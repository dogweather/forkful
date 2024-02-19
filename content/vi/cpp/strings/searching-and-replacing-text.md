---
aliases:
- /vi/cpp/searching-and-replacing-text/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:13.493417-07:00
description: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n l\xE0 vi\u1EC7\
  c t\xECm ki\u1EBFm c\xE1c chu\u1ED7i c\u1EE5 th\u1EC3 trong m\u1ED9t chu\u1ED7i\
  \ l\u1EDBn h\u01A1n v\xE0 thay th\u1EBF ch\xFAng b\u1EB1ng m\u1ED9t c\xE1i g\xEC\
  \ kh\xE1c. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng\u2026"
lastmod: 2024-02-18 23:08:51.027943
model: gpt-4-0125-preview
summary: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n l\xE0 vi\u1EC7c t\xEC\
  m ki\u1EBFm c\xE1c chu\u1ED7i c\u1EE5 th\u1EC3 trong m\u1ED9t chu\u1ED7i l\u1EDB\
  n h\u01A1n v\xE0 thay th\u1EBF ch\xFAng b\u1EB1ng m\u1ED9t c\xE1i g\xEC kh\xE1c.\
  \ C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng\u2026"
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Gì & Tại sao?
Tìm kiếm và thay thế văn bản là việc tìm kiếm các chuỗi cụ thể trong một chuỗi lớn hơn và thay thế chúng bằng một cái gì khác. Các lập trình viên sử dụng nó cho các nhiệm vụ như cập nhật tên biến, sửa đổi dữ liệu, hoặc tự động hóa việc chỉnh sửa trên nhiều tệp.

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

---
aliases:
- /vi/cpp/removing-quotes-from-a-string/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:23.979203-07:00
description: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c kh\u1ECFi m\u1ED9t chu\u1ED7i\
  \ c\xF3 ngh\u0129a l\xE0 g\u1EE1 b\u1ECF nh\u1EEFng k\xFD t\u1EF1 k\xE9p ho\u1EB7\
  c \u0111\u01A1n g\xE2y phi\u1EC1n ph\u1EE9c bao quanh v\u0103n b\u1EA3n c\u1EE7\
  a ch\xFAng ta (' ho\u1EB7c \"). C\xE1c l\u1EADp tr\xECnh vi\xEAn\u2026"
lastmod: 2024-02-18 23:08:51.031141
model: gpt-4-0125-preview
summary: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c kh\u1ECFi m\u1ED9t chu\u1ED7i c\xF3\
  \ ngh\u0129a l\xE0 g\u1EE1 b\u1ECF nh\u1EEFng k\xFD t\u1EF1 k\xE9p ho\u1EB7c \u0111\
  \u01A1n g\xE2y phi\u1EC1n ph\u1EE9c bao quanh v\u0103n b\u1EA3n c\u1EE7a ch\xFA\
  ng ta (' ho\u1EB7c \"). C\xE1c l\u1EADp tr\xECnh vi\xEAn\u2026"
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
---

{{< edit_this_page >}}

## Lý do & Tại sao?
Loại bỏ dấu ngoặc khỏi một chuỗi có nghĩa là gỡ bỏ những ký tự kép hoặc đơn gây phiền phức bao quanh văn bản của chúng ta (' hoặc "). Các lập trình viên thường làm việc này để làm sạch dữ liệu đầu vào, lưu trữ văn bản trong cơ sở dữ liệu, hoặc chuẩn bị chuỗi cho quá trình xử lý tiếp theo mà không cần những dấu ngoặc kéo lê.

## Cách thực hiện:
Dưới đây là một cách thẳng thắn để loại bỏ những dấu ngoặc đó khỏi C++:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Xin chào, 'Thế giới'!")";
    std::string khong_co_dau_nhay = remove_quotes(original);
    std::cout << khong_co_dau_nhay << std::endl;
    return 0;
}
```

Chạy đoạn mã này, và bạn sẽ nhận được:

```
Xin chào, Thế giới!
```

Kì diệu! Các dấu ngoặc đã biến mất.

## Sâu hơn nữa
Dấu ngoặc đã là một rắc rối trong văn bản từ thời sơ khai của việc tính toán. Ngày xưa, bạn sẽ thấy các lập trình viên vất vả lặp qua từng ký tự để lọc bỏ những dấu ngoặc đó. Ngày nay, chúng ta có `std::remove` trong Thư viện Template Chuẩn (STL) để làm phần nặng nhọc.

Có phương án thay thế? Chắc chắn rồi! Bạn có thể sử dụng biểu thức chính quy với `std::regex` để nhắm mục tiêu vào các dấu ngoặc, nhưng đó giống như sử dụng búa đập vào hạt dẻ - mạnh mẽ, nhưng có thể là quá đáng cho những nhiệm vụ đơn giản. Cho những ai ưa chuộng các hương vị C++ gần đây, bạn có thể thử nghiệm với `std::string_view` cho các phương pháp không chỉnh sửa.

Về mặt triển khai, hãy nhớ rằng `std::remove` thực sự không loại bỏ phần tử khỏi container; nó dồn các phần tử không bị loại bỏ về phía trước và trả về một iterator vượt qua phần cuối mới của phạm vi. Đó là lý do tại sao chúng ta cần phương thức `erase` để cắt bỏ phần đuôi không mong muốn.

## Xem thêm
- Tham khảo C++ `std::remove`: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- Thêm về việc điều chỉnh `std::string`: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)

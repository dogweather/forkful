---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:23.979203-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9\
  t c\xE1ch th\u1EB3ng th\u1EAFn \u0111\u1EC3 lo\u1EA1i b\u1ECF nh\u1EEFng d\u1EA5\
  u ngo\u1EB7c \u0111\xF3 kh\u1ECFi C++."
lastmod: '2024-03-13T22:44:37.026853-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t c\xE1ch th\u1EB3ng th\u1EAFn \u0111\
  \u1EC3 lo\u1EA1i b\u1ECF nh\u1EEFng d\u1EA5u ngo\u1EB7c \u0111\xF3 kh\u1ECFi C++."
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
weight: 9
---

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

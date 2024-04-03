---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:24.544591-07:00
description: "C\xE1ch l\xE0m: S\u1EED d\u1EE5ng `<chrono>` v\xE0 `<sstream>` \u0111\
  \u1EC3 ph\xE2n t\xEDch m\u1ED9t ng\xE0y trong C++. D\u01B0\u1EDBi \u0111\xE2y l\xE0\
  \ m\u1ED9t v\xED d\u1EE5 nhanh."
lastmod: '2024-03-13T22:44:37.055121-06:00'
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng `<chrono>` v\xE0 `<sstream>` \u0111\u1EC3 ph\xE2n t\xED\
  ch m\u1ED9t ng\xE0y trong C++."
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xED t\u1EF1"
weight: 30
---

## Cách làm:
Sử dụng `<chrono>` và `<sstream>` để phân tích một ngày trong C++. Dưới đây là một ví dụ nhanh:

```C++
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_text = "2023-04-01";
    std::istringstream ss(date_text);
    std::chrono::year_month_day parsed_date;
    
    ss >> std::chrono::parse("%F", parsed_date);
    if (ss.fail()) {
        std::cout << "Phân tích thất bại\n";
        return 1;
    }

    std::cout << "Năm: " << int(parsed_date.year()) << '\n';
    std::cout << "Tháng: " << unsigned(parsed_date.month()) << '\n';
    std::cout << "Ngày: " << unsigned(parsed_date.day()) << '\n';

    return 0;
}
```

Kết quả Mẫu:
```
Năm: 2023
Tháng: 4
Ngày: 1
```

## Sâu Hơn
Phân tích ngày từ chuỗi không phải là mới. Trở lại những ngày của C, `strptime` là điển hình. Trong C++ hiện đại, `<chrono>` là người bạn đồng hành của bạn. Nó tách biệt các mối quan tâm: định dạng/phân tích với `std::chrono::parse`, và thao tác ngày với các loại `std::chrono`.

Trước C++20, bạn có thể sẽ tìm đến `std::get_time` hoặc các thư viện bên thứ ba như Boost. Sau C++20, thư viện chuẩn có một bản nâng cấp lớn với các cải thiện `std::chrono`. Bây giờ bạn có được các kiểu ngày và chức năng kiểu an toàn ngay từ hộp.

Hàm phân tích, `std::chrono::parse`, linh hoạt, hiểu nhiều định dạng ngày và thời gian. Định dạng "%F" mà chúng tôi đã sử dụng ở trên là định dạng ngày ISO 8601 (năm-tháng-ngày). Nhưng bạn cũng có thể xử lý các định dạng khác, chỉ cần điều chỉnh chuỗi định dạng cho phù hợp.

Nhớ rằng, dù có tính robust trong việc phân tích, đầu vào của người dùng là khó khăn. Luôn xử lý lỗi phân tích một cách nhã nhặn, như đã làm với `ss.fail()` trong ví dụ.

## Xem Thêm
Đào sâu hơn vào `<chrono>` với trang [cppreference chính thức](https://en.cppreference.com/w/cpp/header/chrono).

Nhận được ngữ cảnh lịch sử từ góc nhìn của Stroustrup về lịch sử C++ tại [Thiết kế và Tiến hóa của C++](http://www.stroustrup.com/hopl2.pdf).

Đối với các trường hợp ngoại lệ hoặc định dạng không chuẩn, cân nhắc kiểm tra [Boost.DateTime](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html).

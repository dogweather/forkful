---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:24.544591-07:00
description: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0\
  \ chuy\u1EC3n \u0111\u1ED5i v\u0103n b\u1EA3n th\xE0nh ki\u1EC3u d\u1EEF li\u1EC7\
  u ng\xE0y. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\u1EED\
  \ l\xFD logic li\xEAn quan \u0111\u1EBFn ng\xE0y m\u1ED9t c\xE1ch\u2026"
lastmod: 2024-02-19 22:04:56.258224
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0 chuy\u1EC3\
  n \u0111\u1ED5i v\u0103n b\u1EA3n th\xE0nh ki\u1EC3u d\u1EEF li\u1EC7u ng\xE0y.\
  \ L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\u1EED l\xFD\
  \ logic li\xEAn quan \u0111\u1EBFn ng\xE0y m\u1ED9t c\xE1ch\u2026"
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xED t\u1EF1"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Phân tích ngày từ một chuỗi nghĩa là chuyển đổi văn bản thành kiểu dữ liệu ngày. Lập trình viên làm điều này để xử lý logic liên quan đến ngày một cách tiêu chuẩn, không phụ thuộc vào địa phương, thường dùng cho các nhiệm vụ như xác thực đầu vào, sắp xếp và lưu trữ.

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

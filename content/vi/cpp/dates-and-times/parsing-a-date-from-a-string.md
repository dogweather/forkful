---
title:                "Phân tích ngày từ chuỗi kí tự"
aliases: - /vi/cpp/parsing-a-date-from-a-string.md
date:                  2024-01-28T22:04:24.544591-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày từ chuỗi kí tự"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

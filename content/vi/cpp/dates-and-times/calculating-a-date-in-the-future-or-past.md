---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:53.266021-07:00
description: "L\xE0m sao: C++20 \u0111\xE3 gi\u1EDBi thi\u1EC7u c\xE1c n\xE2ng c\u1EA5\
  p th\u01B0 vi\u1EC7n `<chrono>`, v\xEC v\u1EADy x\u1EED l\xFD th\u1EDDi gian \xED\
  t ph\u1EE9c t\u1EA1p h\u01A1n. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5\
  \ nhanh v\u1EC1 vi\u1EC7c th\xEAm ng\xE0y v\xE0o\u2026"
lastmod: '2024-03-13T22:44:37.060394-06:00'
model: gpt-4-0125-preview
summary: "C++20 \u0111\xE3 gi\u1EDBi thi\u1EC7u c\xE1c n\xE2ng c\u1EA5p th\u01B0 vi\u1EC7\
  n `<chrono>`, v\xEC v\u1EADy x\u1EED l\xFD th\u1EDDi gian \xEDt ph\u1EE9c t\u1EA1\
  p h\u01A1n."
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
weight: 26
---

## Làm sao:
C++20 đã giới thiệu các nâng cấp thư viện `<chrono>`, vì vậy xử lý thời gian ít phức tạp hơn. Dưới đây là một ví dụ nhanh về việc thêm ngày vào ngày hiện tại:

```C++
#include <iostream>
#include <chrono>
#include <format>

using namespace std::chrono;

int main() {
    // Lấy ngày hôm nay
    auto today = floor<days>(system_clock::now());
    
    // Thêm 30 ngày vào hôm nay
    auto future_date = today + days(30);
    
    // Chuyển đổi thành time_point để xuất ra sử dụng system_clock
    auto tp = system_clock::time_point(future_date);
    
    // Xuất ra
    std::cout << "Ngày hôm nay: "
              << std::format("{:%F}\n", today);
    std::cout << "Ngày tương lai (30 ngày sau): "
              << std::format("{:%F}\n", tp);
    return 0;
}
```

Kết quả mẫu:
```
Ngày hôm nay: 2023-03-15
Ngày tương lai (30 ngày sau): 2023-04-14
```

Cách trừ ngày hoạt động tương tự - bạn chỉ sử dụng `-` thay vì `+`.

## Đào sâu
Trước C++20, bạn có thể sẽ sử dụng một thư viện như Boost để thao tác với ngày. Nhưng `<chrono>` được cập nhật đơn giản hóa nó với các kiểu `system_clock`, `year_month_day`, và `duration`.

Trong lịch sử, việc tính toán ngày tháng là phức tạp do việc xử lý thủ công chiều dài tháng biến đổi, năm nhuận, và múi giờ. `<chrono>` của C++20 giải quyết những vấn đề này bằng cách cung cấp hỗ trợ lịch và múi giờ.

Có phương án thay thế không? Bạn vẫn có thể sử dụng Boost hoặc thậm chí tự mình tạo logic ngày tháng (phiêu lưu, nhưng tại sao lại thế?). Cũng có các thư viện bên thứ ba như thư viện "ngày" của Howard Hinnant, đóng một vai trò quan trọng trong các cập nhật chrono của C++20.

Về mặt triển khai, `<chrono>` xác định thời lượng là các hằng số hợp lý tại thời điểm biên dịch, tránh vấn đề về số dấu phẩy động. Các kiểu như `year_month_day` dựa trên `sys_days`, đại diện cho một time_point là số ngày kể từ một kỷ nguyên chung (1970-01-01).

## Xem thêm
- Tham khảo C++ cho `chrono`: https://en.cppreference.com/w/cpp/header/chrono
- Thư viện Ngày của Howard Hinnant (tiền thân cho các cập nhật chrono của C++20): https://github.com/HowardHinnant/date
- Tài liệu về Date/Time của Boost: https://www.boost.org/doc/libs/release/libs/date_time/

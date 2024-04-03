---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:01.138180-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch l\u1EA5\
  y ng\xE0y hi\u1EC7n t\u1EA1i v\u1EDBi `<chrono>`\u2014hi\u1EC7n \u0111\u1EA1i, s\u1EA1\
  ch s\u1EBD, kh\xF4ng l\u1EB1ng nh\u1EB1ng."
lastmod: '2024-03-13T22:44:37.056395-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch l\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1\
  i v\u1EDBi `<chrono>`\u2014hi\u1EC7n \u0111\u1EA1i, s\u1EA1ch s\u1EBD, kh\xF4ng\
  \ l\u1EB1ng nh\u1EB1ng."
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
weight: 29
---

## Làm thế nào:
Dưới đây là cách lấy ngày hiện tại với `<chrono>`—hiện đại, sạch sẽ, không lằng nhằng.

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // Lấy thời gian hệ thống hiện tại
    auto now = std::chrono::system_clock::now();

    // Chuyển đổi sang time_t, sau đó sang tm để có định dạng dễ đọc
    std::time_t now_c = std::chrono::system_clock::to_time_t(now);
    std::tm* now_tm = std::localtime(&now_c);

    // In ra theo định dạng NĂM-THÁNG-NGÀY
    std::cout << (now_tm->tm_year + 1900) << '-' 
              << (now_tm->tm_mon + 1) << '-'
              <<  now_tm->tm_mday << '\n';

    return 0;
}
```

Kết quả mẫu bạn sẽ nhận được hôm nay:
```
2023-4-14
```
Không cần cầu kỳ, thực hiện đúng công việc.

## Tìm hiểu sâu
Ngày xưa, các hàm thời gian của C thống lĩnh—`<ctime>` là lựa chọn hàng đầu của bạn. Nhưng với C++11 và phiên bản sau này, `<chrono>` đã chiếm lấy ánh đèn sân khấu. Nó an toàn về mặt kiểu và tránh được các sai lầm thông thường với các hàm C truyền thống.

Có sự thay thế? Chắc chắn rồi. Bạn có thể sử dụng `std::time` cũ hoặc thậm chí là API cụ thể của OS nếu bạn thích sống mạo hiểm (hoặc có nhu cầu cụ thể).

Và chi tiết thực hiện? `<chrono>` biểu diễn các điểm thời gian, khoảng thời gian, và đồng hồ. Nó chính xác và được thiết kế cẩn thận. Thời gian là điều phức tạp (giây nhuận, múi giờ), và `<chrono>` xử lý sự phức tạp này bên dưới, để bạn không phải lo lắng.

## Xem thêm
- [Tham khảo C++ - thư viện `<chrono>`](https://en.cppreference.com/w/cpp/chrono)
- [Tham khảo C++ - `<ctime>` kiểu cũ](https://en.cppreference.com/w/cpp/header/ctime)
- Để tìm hiểu sâu hơn, hãy xem thư viện ngày của Howard Hinnant, một sự mở rộng của `<chrono>`: [https://github.com/HowardHinnant/date](https://github.com/HowardHinnant/date)
- Nếu bạn cần hỗ trợ múi giờ ngay lập tức, hãy thử cái này: [https://en.cppreference.com/w/cpp/chrono/current_zone](https://en.cppreference.com/w/cpp/chrono/current_zone)

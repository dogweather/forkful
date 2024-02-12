---
title:                "Lấy ngày hiện tại"
aliases: - /vi/cpp/getting-the-current-date.md
date:                  2024-01-28T22:01:01.138180-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lấy ngày hiện tại"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc lấy ngày hiện tại trong chương trình C++ của bạn có thể rất tiện lợi: hãy nghĩ đến việc ghi log, dấu thời gian, hoặc các tính năng lên kế hoạch. Nó giúp phần mềm của bạn luôn phù hợp với thời điểm hiện tại - phần mềm biết hôm nay là ngày nào, giống như bạn vậy.

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

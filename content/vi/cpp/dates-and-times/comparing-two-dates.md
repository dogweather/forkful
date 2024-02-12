---
title:                "So sánh hai ngày"
date:                  2024-01-28T21:57:09.015855-07:00
model:                 gpt-4-0125-preview
simple_title:         "So sánh hai ngày"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
So sánh các ngày là việc xác định xem trong hai ngày nào là sớm hơn, muộn hơn, hoặc nếu chúng giống nhau. Lập trình viên thực hiện việc này để tổ chức sự kiện, hết hạn khuyến mãi, lên lịch, nhắc nhở—cơ bản là, bất cứ thứ gì có thành phần thời gian.

## Cách thực hiện:
C++ làm cho cuộc sống dễ dàng với tiêu đề `<chrono>`.

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    using namespace std::chrono;

    // Tạo các điểm thời gian system_clock
    system_clock::time_point hôm_nay = system_clock::now();
    system_clock::time_point mộtNgàyNàoĐó = system_clock::now() - hours(24); // Hôm qua

    // Chuyển đổi sang time_t để so sánh
    time_t hôm_nay_time_t = system_clock::to_time_t(hôm_nay);
    time_t mộtNgàyNàoĐó_time_t = system_clock::to_time_t(mộtNgàyNàoĐó);

    if (hôm_nay_time_t > mộtNgàyNàoĐó_time_t) {
        std::cout << "Hôm nay là sau mộtNgàyNàoĐó.\n";
    } else if (hôm_nay_time_t < mộtNgàyNàoĐó_time_t) {
        std::cout << "Hôm nay là trước mộtNgàyNàoĐó.\n";
    } else {
        std::cout << "Các ngày giống nhau.\n";
    }

    return 0;
}
```

Kết quả mẫu:

```
Hôm nay là sau mộtNgàyNàoĐó.
```

## Sâu hơn:
Kể từ C++11, `<chrono>` là nơi dành cho ngày và giờ. Trước đó, bạn có thể đang vật lộn với `<ctime>` và các cấu trúc như `tm`. Không đẹp mắt.

Có thay thế không? Chắc chắn, có các thư viện bên thứ ba như Boost.DateTime. Nhưng tại sao phải phức tạp khi `<chrono>` đang ở ngay đó và đang phát triển.

Chi tiết thực hiện để giữ trong túi sau của bạn:
- `std::chrono` đối phó với các điểm thời gian và thời lượng.
- `system_clock` đo thời gian thực tế.
- `time_point` là một điểm thời gian cụ thể (ví dụ, một ngày).
- `time_t` là một kiểu số học, tiện lợi cho việc so sánh.

## Xem thêm:
- Tham khảo C++ cho `<chrono>`: https://en.cppreference.com/w/cpp/header/chrono
- So sánh các thư viện về ngày và thời gian: http://www.boost.org/doc/libs/1_64_0/doc/html/date_time.html
- `<ctime>` quen thuộc, nếu bạn cảm thấy hoài niệm hoặc muốn tự giày vò mình: https://en.cppreference.com/w/cpp/header/ctime

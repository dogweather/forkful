---
aliases:
- /vi/cpp/comparing-two-dates/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:09.015855-07:00
description: "So s\xE1nh c\xE1c ng\xE0y l\xE0 vi\u1EC7c x\xE1c \u0111\u1ECBnh xem\
  \ trong hai ng\xE0y n\xE0o l\xE0 s\u1EDBm h\u01A1n, mu\u1ED9n h\u01A1n, ho\u1EB7\
  c n\u1EBFu ch\xFAng gi\u1ED1ng nhau. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n vi\u1EC7c n\xE0y \u0111\u1EC3 t\u1ED5 ch\u1EE9c s\u1EF1\u2026"
lastmod: 2024-02-18 23:08:51.057911
model: gpt-4-0125-preview
summary: "So s\xE1nh c\xE1c ng\xE0y l\xE0 vi\u1EC7c x\xE1c \u0111\u1ECBnh xem trong\
  \ hai ng\xE0y n\xE0o l\xE0 s\u1EDBm h\u01A1n, mu\u1ED9n h\u01A1n, ho\u1EB7c n\u1EBF\
  u ch\xFAng gi\u1ED1ng nhau. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7\
  c n\xE0y \u0111\u1EC3 t\u1ED5 ch\u1EE9c s\u1EF1\u2026"
title: "So s\xE1nh hai ng\xE0y"
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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:53.525191-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Arduino kh\xF4ng c\xF3 h\xE0m ng\xE0y v\xE0\
  \ gi\u1EDD \u0111\u01B0\u1EE3c x\xE2y d\u1EF1ng s\u1EB5n, nh\u01B0ng b\u1EA1n c\xF3\
  \ th\u1EC3 s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n \"TimeLib.h\" \u0111\u1EC3 x\u1EED\
  \ l\xFD c\xE1c ph\xE9p to\xE1n ng\xE0y. \u0110\u1EA3m b\u1EA3o b\u1EA1n\u2026"
lastmod: '2024-03-13T22:44:37.006827-06:00'
model: gpt-4-0125-preview
summary: "Arduino kh\xF4ng c\xF3 h\xE0m ng\xE0y v\xE0 gi\u1EDD \u0111\u01B0\u1EE3\
  c x\xE2y d\u1EF1ng s\u1EB5n, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5\
  ng th\u01B0 vi\u1EC7n \"TimeLib.h\" \u0111\u1EC3 x\u1EED l\xFD c\xE1c ph\xE9p to\xE1\
  n ng\xE0y."
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
weight: 26
---

## Cách thực hiện:
Arduino không có hàm ngày và giờ được xây dựng sẵn, nhưng bạn có thể sử dụng thư viện "TimeLib.h" để xử lý các phép toán ngày. Đảm bảo bạn đã cài đặt thư viện trước khi sử dụng các ví dụ dưới đây.

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(10, 0, 0, 25, 3, 2023); // Đặt thời gian là ngày 25 tháng 3 năm 2023, 10:00:00
}

void loop() {
  // Tính toán 10 ngày trong tương lai
  time_t futureTime = now() + 10 * SECS_PER_DAY;
  
  // In ngày tương lai
  Serial.print(day(futureTime));
  Serial.print("/");
  Serial.print(month(futureTime));
  Serial.print("/");
  Serial.println(year(futureTime));

  // Tính toán 10 ngày trong quá khứ
  time_t pastTime = now() - 10 * SECS_PER_DAY;
  
  // In ngày quá khứ
  Serial.print(day(pastTime));
  Serial.print("/");
  Serial.print(month(pastTime));
  Serial.print("/");
  Serial.println(year(pastTime));

  // Tránh in liên tục
  delay(10000);
}
```
Kết quả mẫu:
```
4/4/2023
15/3/2023
```

## Tìm hiểu kỹ hơn
Trước khi có các module và thư viện đồng hồ thời gian thực (RTC) và TimeLib, việc giữ thời gian trên Arduino khá đơn giản và thường được triển khai thủ công. Có nhiều cách để tính toán ngày trong tương lai hoặc quá khứ, nhưng sử dụng một thư viện chuyên biệt như TimeLib đơn giản hóa quá trình này đáng kể.

Các phương án thay thế cho TimeLib bao gồm "RTClib.h" toàn diện hơn để sử dụng với các RTC phần cứng, hoặc hàm `millis()` đã xây dựng sẵn để xử lý các khoảng thời gian ngắn hơn (với quản lý ngày thủ công). TimeLib xử lý năm nhuận và múi giờ và cung cấp các hàm tiện ích để dễ dàng thao tác ngày.

Khi tính toán ngày trong tương lai hoặc quá khứ, hãy chú ý đến các múi giờ và thay đổi giờ tiết kiệm ánh sáng ban ngày nếu bạn đang làm việc với các đồng hồ thời gian thực hoặc nguồn thời gian bên ngoài. Trên Arduino, nếu không có RTC hoặc kết nối Internet, bạn sẽ thường xuyên thiết lập thời gian thủ công hoặc qua tín hiệu bên ngoài (như GPS hoặc tín hiệu thời gian radio).

## Xem thêm
- Tài liệu Thư viện Thời gian:
  https://www.arduino.cc/reference/en/libraries/time/
- RTClib, một thư viện phổ biến để làm việc với các đồng hồ thời gian thực:
  https://github.com/adafruit/RTClib
- Hàm millis() của Arduino và các ứng dụng của nó:
  https://www.arduino.cc/reference/en/language/functions/time/millis/

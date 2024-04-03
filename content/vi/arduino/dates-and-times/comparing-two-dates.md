---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:37.154585-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Arduino, b\u1EA1n c\xF3 th\u1EC3\
  \ so s\xE1nh c\xE1c ng\xE0y s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `TimeLib.h`. Tr\u01B0\
  \u1EDBc ti\xEAn, h\xE3y c\xE0i \u0111\u1EB7t n\xF3. Sau \u0111\xF3, h\xE3y xem \u0111\
  o\u1EA1n m\xE3 sau."
lastmod: '2024-03-13T22:44:37.005513-06:00'
model: gpt-4-0125-preview
summary: "Trong Arduino, b\u1EA1n c\xF3 th\u1EC3 so s\xE1nh c\xE1c ng\xE0y s\u1EED\
  \ d\u1EE5ng th\u01B0 vi\u1EC7n `TimeLib.h`."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

## Cách thực hiện:
Trong Arduino, bạn có thể so sánh các ngày sử dụng thư viện `TimeLib.h`. Trước tiên, hãy cài đặt nó. Sau đó, hãy xem đoạn mã sau:

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  // Thiết lập hai thời điểm khác nhau (năm, tháng, ngày, giờ, phút, giây)
  // Ở đây chúng ta đang thiết lập 3 tháng 3 năm 2023, 8:30:00 và 4 tháng 3 năm 2023, 16:45:00
  time_t firstTime = makeTime({0, 30, 8, 3, 3, 2023});
  time_t secondTime = makeTime({0, 45, 16, 4, 3, 2023});
  
  // So sánh hai thời điểm
  if (firstTime < secondTime) {
    Serial.print("Thời điểm đầu tiên sớm hơn.");
  } else if (firstTime > secondTime) {
    Serial.print("Thời điểm thứ hai sớm hơn.");
  } else {
    Serial.print("Cả hai thời điểm đều giống nhau.");
  }
}

void loop() {
  // Không có gì ở đây
}
```

Đầu ra mẫu:
```
Thời điểm đầu tiên sớm hơn.
```

## Sâu hơn
Arduino không có hỗ trợ sẵn cho ngày và giờ, do đó chúng ta sử dụng các thư viện như `TimeLib.h`. Trước khi có thư viện, mọi người phải tự tính và so sánh ngày - một việc khó khăn do các năm nhuận, số ngày khác nhau trong các tháng, v.v.

Cách khác để xử lý ngày tháng bao gồm sử dụng các module Đồng Hồ Thời Gian Thực (RTC), như DS3231, giữ thời gian ngay cả khi Arduino tắt điện. Đối với việc so sánh, bạn vẫn cần kéo các ngày vào chương trình của mình và so sánh chúng giống như chúng ta đã làm ở trên.

Khi triển khai, hãy tính đến các múi giờ và tiết kiệm ánh sáng ban ngày nếu cần thiết. TimeLib có thể xử lý thời gian UTC, giúp né tránh các vấn đề này, nhưng phải chăm sóc đặc biệt với giờ địa phương.

## Xem Thêm
- [Tài liệu Thư viện TimeLib](https://www.pjrc.com/teensy/td_libs_Time.html) - Chi tiết về việc sử dụng thư viện Time.
- [Thư viện Thời gian Arduino](https://github.com/PaulStoffregen/Time) - Kho lưu trữ GitHub cho thư viện Time.

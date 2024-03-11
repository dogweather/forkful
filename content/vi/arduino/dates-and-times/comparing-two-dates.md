---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:37.154585-07:00
description: "So s\xE1nh hai ng\xE0y c\xF3 ngh\u0129a l\xE0 x\xE1c \u0111\u1ECBnh\
  \ xem ng\xE0y n\xE0o tr\u01B0\u1EDBc, ng\xE0y n\xE0o sau, ho\u1EB7c n\u1EBFu ch\xFA\
  ng gi\u1ED1ng nhau. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7\
  c n\xE0y \u0111\u1EC3 theo d\xF5i c\xE1c\u2026"
lastmod: '2024-03-11T00:14:10.302882-06:00'
model: gpt-4-0125-preview
summary: "So s\xE1nh hai ng\xE0y c\xF3 ngh\u0129a l\xE0 x\xE1c \u0111\u1ECBnh xem\
  \ ng\xE0y n\xE0o tr\u01B0\u1EDBc, ng\xE0y n\xE0o sau, ho\u1EB7c n\u1EBFu ch\xFA\
  ng gi\u1ED1ng nhau. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7\
  c n\xE0y \u0111\u1EC3 theo d\xF5i c\xE1c\u2026"
title: "So s\xE1nh hai ng\xE0y"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
So sánh hai ngày có nghĩa là xác định xem ngày nào trước, ngày nào sau, hoặc nếu chúng giống nhau. Các lập trình viên thực hiện việc này để theo dõi các sự kiện dựa trên thời gian, như lên lịch các tác vụ hoặc ghi lại dữ liệu theo thời gian.

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

---
title:                "So sánh hai ngày"
aliases:
- vi/arduino/comparing-two-dates.md
date:                  2024-01-28T21:56:37.154585-07:00
model:                 gpt-4-0125-preview
simple_title:         "So sánh hai ngày"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

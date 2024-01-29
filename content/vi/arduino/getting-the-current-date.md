---
title:                "Lấy ngày hiện tại"
date:                  2024-01-28T22:01:42.886527-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lấy ngày hiện tại"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Lấy ngày hiện tại trên Arduino có nghĩa là truy vấn một đồng hồ thời gian thực (RTC) hoặc dịch vụ thời gian dựa trên internet để biết ngày hiện tại. Tại sao lại làm điều này? Ghi lại các sự kiện, đánh dấu thời gian dữ liệu, hoặc lập lịch cho các hành động—việc biết ngày có thể rất quan trọng cho những công việc này.

## Làm thế nào:

Hãy khiến Arduino của chúng ta thông minh về ngày tháng. Chúng ta sẽ sử dụng một mô-đun RTC, như DS3231, có độ chính xác cao và có pin dự phòng.

```arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  
  if (!rtc.begin()) {
    Serial.println("Không tìm thấy RTC");
    while (1);
  }
  
  if (rtc.lostPower()) {
    Serial.println("RTC mất nguồn, hãy thiết lập thời gian!");
    // dòng sau đây thiết lập RTC với ngày và giờ khi bản sketch này được biên dịch
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  
  delay(3000); // đợi 3 giây trước khi cập nhật ngày
}
```

Kết quả Mẫu:
```
2023/4/5
```

## Tìm hiểu sâu:
Bối cảnh lịch sử? Những máy tính đầu tiên không cần biết ngày tháng. Điều này chỉ trở nên quan trọng khi chúng ta bắt đầu ghi lại và có các hệ thống đa người dùng. Ngày nay, điều đó đã trở nên được kỳ vọng.

Các phương án thay thế cho RTCs bao gồm sử dụng Giao thức Thời gian Mạng (NTP) khi kết nối với internet, hoặc các mô-đun GPS cung cấp thông tin thời gian và ngày chính xác.

Chi tiết thực hiện quan trọng. Không phải tất cả RTCs đều được tạo ra như nhau. Một số, như DS1307, ít chính xác hơn và có thể bị lệch hơn theo thời gian. Thư viện như `RTClib.h` che giấu sự khác biệt giữa các mô-đun, làm cho cuộc sống của bạn dễ dàng hơn.

Sử dụng NTP qua WiFi đòi hỏi một cách tiếp cận khác. Bạn cần một ESP8266 hoặc ESP32 với truy cập internet, và bao gồm các thư viện như `WiFi.h` và `NTPClient.h`. Mẫu mã code thay đổi—bạn thực hiện các yêu cầu định kỳ tới một máy chủ thời gian và phân tích kết quả cho ngày tháng.

## Xem thêm:
- [Thư viện RTClib](https://github.com/adafruit/RTClib): Một thư viện giúp giao tiếp với các RTC trở nên dễ dàng.
- [Tờ dữ liệu DS3231](https://datasheets.maximintegrated.com/en/ds/DS3231.pdf): Chi tiết tỉ mỉ về mô-đun RTC DS3231.
- [Thư viện NTPClient](https://github.com/arduino-libraries/NTPClient): Để lấy thời gian qua internet.
- [Thời gian và Ngày trên Arduino Không Cần RTC](https://create.arduino.cc/projecthub/Arnov_Sharma_makes/time-and-date-on-arduino-without-a-rtc-module-c7d2d6): Các phương pháp thay thế nếu bạn không sử dụng RTC.

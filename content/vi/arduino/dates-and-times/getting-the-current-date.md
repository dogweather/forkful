---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:42.886527-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y khi\u1EBFn Arduino c\u1EE7a ch\xFAng\
  \ ta th\xF4ng minh v\u1EC1 ng\xE0y th\xE1ng. Ch\xFAng ta s\u1EBD s\u1EED d\u1EE5\
  ng m\u1ED9t m\xF4-\u0111un RTC, nh\u01B0 DS3231, c\xF3 \u0111\u1ED9 ch\xEDnh x\xE1\
  c cao v\xE0 c\xF3 pin d\u1EF1\u2026"
lastmod: '2024-03-13T22:44:37.002876-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y khi\u1EBFn Arduino c\u1EE7a ch\xFAng ta th\xF4ng minh v\u1EC1 ng\xE0\
  y th\xE1ng."
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
weight: 29
---

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

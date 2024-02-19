---
aliases:
- /vi/arduino/converting-a-date-into-a-string/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:24.957930-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i ng\xE0y th\xE0nh chu\u1ED7i ngh\u0129a l\xE0\
  \ thay \u0111\u1ED5i bi\u1EC3u di\u1EC5n c\u1EE7a ng\xE0y t\u1EEB m\u1ED9t \u0111\
  \u1ECBnh d\u1EA1ng m\xE0 l\u1EADp tr\xECnh hi\u1EC3u, nh\u01B0 s\u1ED1 nguy\xEA\
  n c\u1EE7a ng\xE0y, th\xE1ng v\xE0 n\u0103m, sang v\u0103n b\u1EA3n\u2026"
lastmod: 2024-02-18 23:08:51.011725
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i ng\xE0y th\xE0nh chu\u1ED7i ngh\u0129a l\xE0 thay\
  \ \u0111\u1ED5i bi\u1EC3u di\u1EC5n c\u1EE7a ng\xE0y t\u1EEB m\u1ED9t \u0111\u1ECB\
  nh d\u1EA1ng m\xE0 l\u1EADp tr\xECnh hi\u1EC3u, nh\u01B0 s\u1ED1 nguy\xEAn c\u1EE7\
  a ng\xE0y, th\xE1ng v\xE0 n\u0103m, sang v\u0103n b\u1EA3n\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Chuyển đổi ngày thành chuỗi nghĩa là thay đổi biểu diễn của ngày từ một định dạng mà lập trình hiểu, như số nguyên của ngày, tháng và năm, sang văn bản thông thường. Chúng ta làm điều này để hiển thị ngày tháng theo định dạng dễ đọc cho con người hoặc để chuẩn bị lưu trữ và sử dụng sau này.

## Cách thực hiện:

Dưới đây là một ví dụ đơn giản về cách chuyển đổi ngày thành chuỗi trên Arduino:

```Arduino
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Không tìm thấy RTC");
    while (1);
  }
  
  DateTime now = rtc.now();
  char dateString[11]; // Đủ không gian cho "DD/MM/YYYY"

  sprintf(dateString, "%02d/%02d/%04d", now.day(), now.month(), now.year());
  Serial.println(dateString);
}

void loop() {
  // Không cần phải lặp lại việc chuyển đổi.
}
```

Đầu ra mẫu:

```
23/03/2023
```

## Sâu xa hơn

Lịch sử, việc biểu diễn thời gian đã là một khía cạnh phức tạp của lập trình do có nhiều định dạng và múi giờ khác nhau. Các chức năng liên quan đến thời gian của Arduino đã giải quyết sự phức tạp này, cho phép chúng ta tập trung vào việc làm thế nào để hiểu dữ liệu thời gian.

Dù chúng ta đã sử dụng thư viện `RTClib`, các lựa chọn thay thế như `TimeLib.h` cung cấp chức năng tương tự. Việc chọn lựa phụ thuộc vào sở thích cá nhân và các tính năng cụ thể, như xử lý múi giờ tích hợp.

Chức năng quan trọng `sprintf` được sử dụng ở đây định dạng dữ liệu thành chuỗi. Nó dựa trên hàm của thư viện chuẩn C, rất mạnh mẽ nhưng có thể tốn nhiều bộ nhớ khi sử dụng phức tạp. Một lựa chọn nhẹ nhàng, cơ bản hơn sẽ là `snprintf`, giúp đảm bảo bạn không vượt quá kích thước bộ đệm và an toàn hơn chống tràn bộ đệm.

## Xem thêm

- Thư viện Time của Arduino: http://playground.arduino.cc/Code/Time
- DateFormat: https://www.arduino.cc/reference/en/libraries/date-format/
- Tài liệu RTClib: https://github.com/adafruit/RTClib

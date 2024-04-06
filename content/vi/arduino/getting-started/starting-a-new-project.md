---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:23.919323-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: K\u1EBFt n\u1ED1i b\u1EA3ng Arduino c\u1EE7\
  a b\u1EA1n, t\u1EA3i l\xEAn b\u1EA3n sketch v\xE0 quan s\xE1t LED n\u1ED9i b\u1ED9\
  \ nh\u1EA5p nh\xE1y m\u1ED7i gi\xE2y."
lastmod: '2024-04-05T21:53:38.352591-06:00'
model: gpt-4-0125-preview
summary: "K\u1EBFt n\u1ED1i b\u1EA3ng Arduino c\u1EE7a b\u1EA1n, t\u1EA3i l\xEAn b\u1EA3\
  n sketch v\xE0 quan s\xE1t LED n\u1ED9i b\u1ED9 nh\u1EA5p nh\xE1y m\u1ED7i gi\xE2\
  y."
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
weight: 1
---

## Cách thực hiện:
```Arduino
// Tạo một bản sketch Blink đơn giản để bắt đầu một dự án Arduino mới

void setup() {
  pinMode(LED_BUILTIN, OUTPUT); // Đặt LED nội bộ thành đầu ra
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH); // Bật LED
  delay(1000);                     // Đợi một giây
  digitalWrite(LED_BUILTIN, LOW);  // Tắt LED
  delay(1000);                     // Đợi thêm một giây nữa
}
```

Kết nối bảng Arduino của bạn, tải lên bản sketch và quan sát LED nội bộ nhấp nháy mỗi giây.

## Tìm hiểu sâu
Khi bạn bắt đầu một dự án Arduino mới, bạn đang bước theo dấu chân của vô số phát minh và những người chế tạo. Arduino bắt đầu vào năm 2005 tại Ivrea, Ý, như một công cụ dành cho sinh viên không có nền tảng về điện tử và lập trình. Kể từ đó, nó đã trở thành một phần không thể thiếu trong điện tử tự làm, nguyên mẫu và lập trình giáo dục.

Có những lựa chọn thay thế để bắt đầu một dự án từ đầu. Bạn có thể chỉnh sửa mã hiện có hoặc sử dụng thư viện để thêm các tính năng phức tạp mà không cần phải phát minh lại bánh xe - nhưng không có gì vượt qua được sự phấn khích khi tạo ra thứ gì đó hoàn toàn là của bạn.

Bản sketch bắt đầu với hàm `setup()`, chạy một lần để thiết lập phần cứng của bạn, tiếp theo là hàm `loop()`, chạy liên tục, cho phép bạn kiểm soát hành vi của dự án. Làm chủ cách sử dụng và cấu trúc của những hàm này, và bạn đang trên đường trở thành một chuyên gia Arduino.

## Xem Thêm
- Tài liệu chính thức về Arduino: https://www.arduino.cc/reference/en/
- Giới thiệu về bản Sketch Arduino: https://www.arduino.cc/en/Tutorial/BuiltInExamples
- Diễn đàn Arduino – Hướng dẫn Dự án: https://forum.arduino.cc/c/project-guidance/8

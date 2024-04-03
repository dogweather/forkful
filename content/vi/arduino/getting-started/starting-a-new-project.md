---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:23.919323-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: ."
lastmod: '2024-03-13T22:44:36.991009-06:00'
model: gpt-4-0125-preview
summary: .
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

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:07.518368-07:00
description: "L\xFD do l\xE0 g\xEC? G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDB\
  i x\xE1c th\u1EF1c c\u01A1 b\u1EA3n th\xEAm m\u1ED9t t\u1EA7ng b\u1EA3o m\u1EAD\
  t b\u1EB1ng c\xE1ch y\xEAu c\u1EA7u t\xEAn ng\u01B0\u1EDDi d\xF9ng v\xE0 m\u1EAD\
  t kh\u1EA9u. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 \u0111\u1EC3\u2026"
lastmod: '2024-03-13T22:44:36.989985-06:00'
model: gpt-4-0125-preview
summary: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1\
  \ b\u1EA3n th\xEAm m\u1ED9t t\u1EA7ng b\u1EA3o m\u1EADt b\u1EB1ng c\xE1ch y\xEA\
  u c\u1EA7u t\xEAn ng\u01B0\u1EDDi d\xF9ng v\xE0 m\u1EADt kh\u1EA9u."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

## Lý do là gì?
Gửi một yêu cầu HTTP với xác thực cơ bản thêm một tầng bảo mật bằng cách yêu cầu tên người dùng và mật khẩu. Lập trình viên sử dụng nó để truy cập các API hoặc dịch vụ web chỉ dành cho người dùng được ủy quyền.

## Làm thế nào:
Để thực hiện điều này trên Arduino, trước tiên bạn cần bao gồm các thư viện cần thiết - thường là `<ESP8266WiFi.h>` cho ESP8266 hoặc `<WiFi.h>` cho ESP32, và `<Base64.h>` để mã hóa các chi tiết xác thực. Dưới đây là đoạn mã cơ bản để bạn bắt đầu:

```Arduino
#include <ESP8266WiFi.h>
#include <Base64.h>

const char* ssid = "yourSSID";
const char* password = "yourPassword";
const char* server = "your.server.com";
const char* authUser = "user";
const char* authPass = "pass";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  
  String auth = "Basic " + base64::encode(String(authUser) + ":" + String(authPass));

  WiFiClient client;
  if (client.connect(server, 80)) {
    client.println("GET /route HTTP/1.1");
    client.print("Host: ");
    client.println(server);
    client.println("Authorization: " + auth);
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  // Đặt mã thông thường của bạn ở đây
}
```

Khi chạy, Arduino sẽ kết nối với máy chủ chỉ định với thông tin đăng nhập và lấy nội dung được bảo vệ.

## Đi sâu hơn
Xác thực HTTP cơ bản đã có từ những ngày đầu của web, được định nghĩa vào năm 1996 bởi RFC 2617. Nó đơn giản: mã hóa tên người dùng và mật khẩu trong base64 và đặt nó vào một tiêu đề HTTP. Đây không phải là phương pháp bảo mật nhất (vì base64 dễ dàng đảo ngược), nhưng nó đơn giản cho các công cụ ít rủi ro hoặc nội bộ.

Có các phương pháp thay thế, như Xác thực Truy cập Digest hoặc OAuth, là an toàn hơn, nhưng chúng cũng tốn nhiều tài nguyên hơn - điều này cần được xem xét trên một Arduino nhỏ.

Khi triển khai, hãy nhớ rằng mã hóa base64 tăng kích thước của thông tin xác thực khoảng 33%, và bộ nhớ của Arduino bị giới hạn. Cũng, đảm bảo máy chủ của bạn sử dụng SSL/TLS (HTTPS) nếu bạn gửi thông tin xác thực qua internet để tránh tiết lộ.

## Xem thêm
- [Wikipedia về xác thực truy cập cơ bản](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Bảo mật yêu cầu HTTP của bạn](https://arduino.cc/en/Tutorial/WebClientRepeating)

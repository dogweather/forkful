---
title:                "Gửi một yêu cầu HTTP với xác thực cơ bản"
aliases:
- vi/arduino/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-28T22:08:07.518368-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP với xác thực cơ bản"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gửi một yêu cầu HTTP với xác thực cơ bản với Arduino

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

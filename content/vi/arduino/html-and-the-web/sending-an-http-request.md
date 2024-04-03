---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:07.492837-07:00
description: "L\xE0m th\u1EBF n\xE0o: L\xE0m vi\u1EC7c v\u1EDBi Arduino y\xEAu c\u1EA7\
  u th\u01B0 vi\u1EC7n `WiFiNINA` cho c\xE1c t\xEDnh n\u0103ng m\u1EA1ng. D\u01B0\u1EDB\
  i \u0111\xE2y l\xE0 c\xE1ch g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u GET \u0111\u01A1n\
  \ gi\u1EA3n."
lastmod: '2024-03-13T22:44:36.986497-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi Arduino y\xEAu c\u1EA7u th\u01B0 vi\u1EC7n `WiFiNINA`\
  \ cho c\xE1c t\xEDnh n\u0103ng m\u1EA1ng."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Làm thế nào:
Làm việc với Arduino yêu cầu thư viện `WiFiNINA` cho các tính năng mạng. Dưới đây là cách gửi một yêu cầu GET đơn giản:

```Arduino
#include <WiFiNINA.h>

char ssid[] = "yourNetworkName";       // tên SSID (tên) của mạng của bạn
char pass[] = "yourNetworkPass";       // mật khẩu mạng của bạn
int status = WL_IDLE_STATUS;           // trạng thái của radio WiFi
char server[] = "example.com";         // máy chủ bạn muốn kết nối tới

WiFiClient client;

void setup() {
  Serial.begin(9600);                  // bắt đầu serial để gỡ lỗi
  WiFi.begin(ssid, pass);              // bắt đầu kết nối WiFi
  while (status != WL_CONNECTED) {     // đợi cho tới khi kết nối:
    status = WiFi.status();
    delay(1000);
  }
  Serial.print("Đã kết nối tới ");
  Serial.println(ssid);
}

void loop() {
  if (client.connect(server, 80)) {    // nếu bạn kết nối được, gửi yêu cầu:
    client.println("GET / HTTP/1.1");
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println();                   // kết thúc yêu cầu
  } else {
    Serial.println("Kết nối thất bại"); // nếu bạn không kết nối được với máy chủ:
  }

  while (client.connected()) {         // trong khi bạn đang kết nối, đọc dữ liệu:
    if (client.available()) {
      char c = client.read();
      Serial.print(c);
    }
  }

  if (!client.connected()) {           // nếu máy chủ ngắt kết nối, ngừng client:
    client.stop();
  }

  delay(10000);                        // đợi mười giây trước khi thử lại
}
```

Output Mẫu:
```
HTTP/1.1 200 OK
Ngày: Thứ Hai, 23 Tháng 1 2023 12:36:47 GMT
Máy chủ: Apache/2.4.1 (Unix)
...
```

## Sâu Hơn
Khái niệm gửi một yêu cầu HTTP từ một vi điều khiển không phải lúc nào cũng có. Trong quá khứ, vi điều khiển thường được biết đến với các cảm biến và tương tác với thế giới vật lý. Nhưng với sự xuất hiện của IoT (Internet of Things), những thiết bị này bắt đầu cần kết nối web. Arduino giờ đây có thể sử dụng các thư viện như `WiFiNINA` để xử lý những kết nối này một cách mạnh mẽ.

Tùy thuộc vào phần cứng của bạn, có những lựa chọn thay thế cho `WiFiNINA`. Chẳng hạn, thư viện `Ethernet` tận dụng các kết nối có dây, trong khi `WiFi101` làm việc với các lá chắn WiFi cũ hơn.

Về mặt triển khai, việc tạo một yêu cầu HTTP có vẻ đơn giản, nhưng việc bắt tay, tiêu đề và các phương thức HTTP (GET, POST, v.v.) là một phần của một giao thức nghiêm ngặt cho phép các thiết bị giao tiếp qua web. Arduino giấu đi phần lớn sự phức tạp này, nhưng việc hiểu biết cơ bản giúp giải quyết sự cố khi mọi thứ không diễn ra suôn sẻ.

## Xem Thêm
- Tài liệu thư viện `WiFiNINA` của Arduino: https://www.arduino.cc/en/Reference/WiFiNINA
- Primer giao thức HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP
- Trung tâm dự án Arduino cho các dự án kết nối web: https://create.arduino.cc/projecthub

---
title:                "Gửi một yêu cầu HTTP"
aliases:
- /vi/arduino/sending-an-http-request/
date:                  2024-01-28T22:08:07.492837-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Gửi một yêu cầu HTTP là cách mà Arduino của bạn giao tiếp với web, như yêu cầu một máy chủ gửi lại một số dữ liệu. Các lập trình viên thực hiện điều này để cho phép Arduino của họ tương tác với các API, tải nội dung web hoặc giao tiếp với các dịch vụ dựa trên internet khác.

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

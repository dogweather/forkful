---
title:                "Phân Tích Cú Pháp HTML"
date:                  2024-01-28T22:03:58.903387-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân Tích Cú Pháp HTML"

category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Điều gì và Tại sao?
Phân tích cú pháp HTML có nghĩa là lọc qua mã HTML để trích xuất các phần hữu ích - giống như tìm số điện thoại trên một trang liên hệ. Tại sao lại làm điều này? Để tự động hóa việc thu thập dữ liệu hoặc tương tác với các trang web từ dự án Arduino của bạn.

## Cách thực hiện:
Arduino không tự nhiên thuần thục về web, nhưng với các module bên ngoài (như ESP8266), bạn có thể kết nối và lấy nội dung web. Ở đây chúng ta sẽ kéo HTML và tìm kiếm một thẻ cụ thể:

```Arduino
#include <ESP8266WiFi.h>
#include <WiFiClient.h>

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";

const char* host = "example.com";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  WiFiClient client;
  
  nếu (!client.connect(host, 80)) {
    Serial.println("Kết nối thất bại");
    return;
  }
  
  client.println("GET / HTTP/1.1");
  client.print("Host: ");
  client.println(host);
  client.println("Đóng kết nối");
  client.println();

  while (client.connected() || client.available()) {
    if (client.available()) {
      String line = client.readStringUntil('\n');
      if (line.indexOf("<title>") >= 0) {
        int startIndex = line.indexOf("<title>") + 7;
        int endIndex = line.indexOf("</title>");
        String pageTitle = line.substring(startIndex, endIndex);
        Serial.println(pageTitle);
      }
    }
  }
}

void loop() {
  // Chúng ta chạy cài đặt một lần và lấy thông tin chúng ta đang tìm kiếm. Không cần lặp lại.
}
```

Kết quả Mẫu:
```
Example Domain
```

## Đào Sâu:
Lịch sử, microcontrollers như Arduino không được thiết kế để thực hiện các tác vụ phức tạp như phân tích cú pháp HTML. Mọi thứ thay đổi với các module và thư viện có khả năng mạng làm phong phú thêm khả năng của chúng.

Chìa khóa để phân tích cú pháp HTML là thao tác chuỗi. Bạn đang tìm kiếm mẫu. Nhưng nhớ rằng, HTML có thể rất lộn xộn. Nó không giống như JSON với cấu trúc đáng tin cậy. Phương pháp này hoạt động cho các tác vụ đơn giản nhưng có thể thất bại nếu HTML thay đổi một cách bất ngờ.

Có phương pháp thay thế không? Chắc chắn rồi. Nếu bạn nghiêm túc về việc phân tích cú pháp, hãy xem xét một microcontroller tương thích với Arduino có nhiều sức mạnh hơn hoặc những loại có thể chạy Linux, mở ra công cụ như Python với thư viện được thiết kế cho việc lấy dữ liệu web.

Sự đơn giản của Arduino là một lợi ích và cũng là một hạn chế ở đây. Bạn có thể thực hiện việc phân tích cú pháp cơ bản mà không cần nhiều trục trặc, nhưng nếu bạn cần xử lý HTML phức tạp hoặc lượng dữ liệu lớn, bạn đã vượt quá khả năng của Uno.

## Xem Thêm:
- [Kho lưu trữ GitHub ESP8266](https://github.com/esp8266/Arduino)
- [Thư viện Arduino HttpClient](https://github.com/arduino-libraries/ArduinoHttpClient)
- [Lấy dữ liệu web với Python](https://realpython.com/python-web-scraping-practical-introduction/)

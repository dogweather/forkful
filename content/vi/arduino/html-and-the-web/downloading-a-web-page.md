---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:16.744375-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 chi ti\u1EBF\
  t: l\xE0m cho Arduino c\u1EE7a b\u1EA1n l\u01B0\u1EDBt web v\xE0 thu th\u1EADp nh\u1EEF\
  ng g\xEC b\u1EA1n c\u1EA7n."
lastmod: '2024-03-13T22:44:36.989062-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 chi ti\u1EBFt."
title: "T\u1EA3i trang web"
weight: 42
---

## Cách thực hiện:
Dưới đây là chi tiết: làm cho Arduino của bạn lướt web và thu thập những gì bạn cần.

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Đang kết nối với WiFi...");
  }

  HTTPClient http;
  http.begin("http://example.com"); // Đổi lại URL của bạn
  
  int httpCode = http.GET();
  
  if (httpCode > 0) {
    if (httpCode == HTTP_CODE_OK) {
      String payload = http.getString();
      Serial.println(payload);
    }
  } else {
    Serial.printf("Lỗi trong yêu cầu HTTP: %s\n", http.errorToString(httpCode).c_str());
  }
  http.end();
}

void loop() {
  // Không có gì ở đây tạm thời.
}
```

Khởi động, và bạn sẽ thấy HTML của trang web trong Serial Monitor. Nhớ rằng, bạn sẽ cần mô-đun Wi-Fi ESP8266 và một kết nối.

## Sâu hơn nữa
Ngày xưa, Arduinos là những sinh vật đơn giản không kết nối mạng. Sau đó, các shield và module đã kết nối chúng với mạng lớn khó lường. ESP8266 là một phép màu như vậy, biến Arduino của bạn thành một người lướt internet.

Có lựa chọn khác không? Chắc chắn rồi. Có ESP32, Ethernet Shield, và các lựa chọn khác cho cùng một công việc.

Chất lượng của kết nối internet, sự mạnh mẽ của nguồn cấp điện, và thậm chí thời gian trong ngày có thể ảnh hưởng đến việc Arduino của bạn tải trang đó như thế nào. Thực sự chúng ta đang kết nối với nhiều yếu tố hơn là chỉ viết code sành điệu.

## Xem Thêm
Muốn khám phá thêm? Hãy xem những điều này:

- [Mạng Arduino](https://www.arduino.cc/en/Guide/ArduinoEthernetShield)
- [Wiki GitHub ESP8266](https://github.com/esp8266/Arduino)
- [Kho GitHub ESP32](https://github.com/espressif/arduino-esp32)

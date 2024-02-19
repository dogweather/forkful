---
aliases:
- /vi/arduino/downloading-a-web-page/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:16.744375-07:00
description: "T\u1EA3i xu\u1ED1ng m\u1ED9t trang web ngh\u0129a l\xE0 l\u1EA5y n\u1ED9\
  i dung HTML t\u1EEB URL b\u1EA1n \u0111ang xem. C\xE1c l\u1EADp tr\xECnh vi\xEA\
  n l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 k\xE9o d\u1EEF li\u1EC7u, c\u1EADp nh\u1EAD\
  t gadget c\u1EE7a h\u1ECD, ho\u1EB7c \u0111\u01A1n\u2026"
lastmod: 2024-02-18 23:08:50.998938
model: gpt-4-0125-preview
summary: "T\u1EA3i xu\u1ED1ng m\u1ED9t trang web ngh\u0129a l\xE0 l\u1EA5y n\u1ED9\
  i dung HTML t\u1EEB URL b\u1EA1n \u0111ang xem. C\xE1c l\u1EADp tr\xECnh vi\xEA\
  n l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 k\xE9o d\u1EEF li\u1EC7u, c\u1EADp nh\u1EAD\
  t gadget c\u1EE7a h\u1ECD, ho\u1EB7c \u0111\u01A1n\u2026"
title: "T\u1EA3i trang web"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Tải xuống một trang web nghĩa là lấy nội dung HTML từ URL bạn đang xem. Các lập trình viên làm điều này để kéo dữ liệu, cập nhật gadget của họ, hoặc đơn giản là sử dụng internet nhiều hơn là chỉ xem video mèo.

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

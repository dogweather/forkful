---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:37.938451-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi JSON\
  \ trong Arduino, b\u1EA1n s\u1EBD c\u1EA7n th\u01B0 vi\u1EC7n ArduinoJson. C\xE0\
  i \u0111\u1EB7t n\xF3 th\xF4ng qua Qu\u1EA3n l\xFD Th\u01B0 vi\u1EC7n: Sketch >\
  \ Include Library > Manage\u2026"
lastmod: '2024-03-13T22:44:37.017331-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi JSON trong Arduino, b\u1EA1n s\u1EBD\
  \ c\u1EA7n th\u01B0 vi\u1EC7n ArduinoJson."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

## Làm thế nào:
Để làm việc với JSON trong Arduino, bạn sẽ cần thư viện ArduinoJson. Cài đặt nó thông qua Quản lý Thư viện: Sketch > Include Library > Manage Libraries... sau đó tìm kiếm "ArduinoJson" và cài đặt.

Dưới đây là một ví dụ đơn giản để phân tích JSON:

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);

  DynamicJsonDocument doc(1024);
  deserializeJson(doc, json);

  const char* sensor = doc["sensor"];
  long time = doc["time"];
  double vĩ độ = doc["data"][0];
  double kinh độ = doc["data"][1];
  
  Serial.print("Cảm biến: ");
  Serial.println(sensor);
  Serial.print("Thời gian: ");
  Serial.println(time);
  Serial.print("Vĩ độ: ");
  Serial.println(vĩ độ, 6);
  Serial.print("Kinh độ: ");
  Serial.println(kinh độ, 6);
}

void loop() {
  // Không sử dụng trong ví dụ này.
}
```

Kết quả mẫu:

```
Cảm biến: gps
Thời gian: 1351824120
Vĩ độ: 48.756080
Kinh độ: 2.302038
```

Tạo JSON:

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  DynamicJsonDocument doc(1024);

  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  doc["data"][0] = 48.756080;
  doc["data"][1] = 2.302038;

  serializeJson(doc, Serial);
}

void loop() {
  // Không sử dụng trong ví dụ này.
}
```

Kết quả mẫu:

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

## Sâu hơn
Thư viện ArduinoJson, bởi Benoit Blanchon, trở thành tiêu chuẩn de facto cho việc thao tác JSON trong Arduino. JSON trở nên phổ biến vì sự đơn giản so với XML, được sử dụng rộng rãi trước đó. Các lựa chọn thay thế như MsgPack tồn tại nhưng JSON vẫn được yêu thích vì độ dễ đọc văn bản và sử dụng rộng rãi. Về cách thực hiện, hãy đảm bảo bạn cấp phát đủ bộ nhớ cho `DynamicJsonDocument` để tránh tràn và sử dụng `StaticJsonDocument` cho các đối tượng JSON tĩnh hoặc có kích thước cố định.

## Xem thêm
- Tài liệu Thư viện ArduinoJson: https://arduinojson.org/
- Website Chính thức của JSON: https://www.json.org/json-en.html
- Diễn đàn Arduino để Thảo luận: https://forum.arduino.cc/
- Hướng dẫn Chọn giữa StaticJsonDocument và DynamicJsonDocument: https://arduinojson.org/documentation/memory-model/

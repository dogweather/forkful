---
aliases:
- /vi/arduino/working-with-json/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:37.938451-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi JSON (JavaScript Object Notation) bao g\u1ED3\
  m vi\u1EC7c thao t\xE1c d\u1EEF li\u1EC7u \u0111\u01B0\u1EE3c c\u1EA5u tr\xFAc trong\
  \ m\u1ED9t \u0111\u1ECBnh d\u1EA1ng d\u1EF1a tr\xEAn v\u0103n b\u1EA3n, nh\u1EB9\
  , d\u1EC5 \u0111\u1ECDc v\xE0 vi\u1EBFt cho\u2026"
lastmod: 2024-02-18 23:08:51.022790
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi JSON (JavaScript Object Notation) bao g\u1ED3\
  m vi\u1EC7c thao t\xE1c d\u1EEF li\u1EC7u \u0111\u01B0\u1EE3c c\u1EA5u tr\xFAc trong\
  \ m\u1ED9t \u0111\u1ECBnh d\u1EA1ng d\u1EF1a tr\xEAn v\u0103n b\u1EA3n, nh\u1EB9\
  , d\u1EC5 \u0111\u1ECDc v\xE0 vi\u1EBFt cho\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm việc với JSON (JavaScript Object Notation) bao gồm việc thao tác dữ liệu được cấu trúc trong một định dạng dựa trên văn bản, nhẹ, dễ đọc và viết cho con người, và dễ dàng cho máy móc phân tích và tạo. Các lập trình viên sử dụng JSON trong dự án Arduino để giao tiếp với các dịch vụ web, trao đổi dữ liệu, và cấu hình thiết bị một cách liền mạch.

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

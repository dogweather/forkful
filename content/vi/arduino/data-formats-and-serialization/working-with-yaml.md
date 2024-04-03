---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:36.700769-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Arduino kh\xF4ng h\u1ED7 tr\u1EE3 YAML\
  \ ngay t\u1EEB \u0111\u1EA7u. \u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi n\xF3, b\u1EA1\
  n s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n b\xEAn ngo\xE0i. V\xED d\u1EE5: C\xE0i \u0111\
  \u1EB7t th\u01B0 vi\u1EC7n \"ArduinoJson\" th\xF4ng qua\u2026"
lastmod: '2024-03-13T22:44:37.016099-06:00'
model: gpt-4-0125-preview
summary: "Arduino kh\xF4ng h\u1ED7 tr\u1EE3 YAML ngay t\u1EEB \u0111\u1EA7u."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Cách thực hiện:
Arduino không hỗ trợ YAML ngay từ đầu. Để làm việc với nó, bạn sử dụng thư viện bên ngoài. Ví dụ:

Cài đặt thư viện "ArduinoJson" thông qua Library Manager. Sử dụng `DynamicJsonDocument` để phân tích cú pháp:

```Arduino
#include <ArduinoJson.h>

const char* yaml = 
  "- title: The Catcher in the Rye\n"
  "  author: J.D. Salinger\n"
  "- title: Nineteen Eighty-Four\n"
  "  author: George Orwell\n";

void setup() {
  Serial.begin(9600);
  DynamicJsonDocument doc(1024);
  deserializeJson(doc, yaml);
  for (JsonObject elem : doc.as<JsonArray>()) {
    Serial.println(elem["title"].as<String>());
    Serial.println(elem["author"].as<String>());
  }
}

void loop() {
  // không được sử dụng trong ví dụ này
}
```

Kết quả mẫu:

```
The Catcher in the Rye
J.D. Salinger
Nineteen Eighty-Four
George Orwell
```

## Đi sâu hơn
YAML xuất hiện vào đầu những năm 2000, được xây dựng cho tính dễ đọc của con người. Là một bản mở rộng của JSON, bất kỳ tệp JSON nào cũng là một tệp YAML hợp lệ. Các lựa chọn thay thế phổ biến bao gồm JSON hoặc XML, nhưng cú pháp tối thiểu của YAML nhằm mục đích quản lý tốt hơn bởi con người mà không cần thêm phô trương. Việc phân tích cú pháp YAML trên Arduino nghĩa là chuyển đổi YAML sang JSON sử dụng các công cụ bên ngoài và sau đó sử dụng JSON trong các bản vẽ của bạn.

## Xem thêm
- Website chính thức của YAML: https://yaml.org
- Kho lưu trữ GitHub của ArduinoJson: https://github.com/bblanchon/ArduinoJson
- Chuyển đổi trực tuyến từ YAML sang JSON: https://www.json2yaml.com/

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:36.700769-07:00
description: "YAML kh\xF4ng ph\u1EA3i l\xE0 ng\xF4n ng\u1EEF \u0111\xE1nh d\u1EA5\
  u. N\xF3 l\xE0 m\u1ED9t ti\xEAu chu\u1EA9n tu\u1EA7n t\u1EF1 h\xF3a d\u1EEF li\u1EC7\
  u th\xE2n thi\u1EC7n v\u1EDBi con ng\u01B0\u1EDDi cho t\u1EA5t c\u1EA3 c\xE1c ng\xF4\
  n ng\u1EEF l\u1EADp tr\xECnh. C\xE1c l\u1EADp tr\xECnh\u2026"
lastmod: '2024-03-13T22:44:37.016099-06:00'
model: gpt-4-0125-preview
summary: "YAML kh\xF4ng ph\u1EA3i l\xE0 ng\xF4n ng\u1EEF \u0111\xE1nh d\u1EA5u. N\xF3\
  \ l\xE0 m\u1ED9t ti\xEAu chu\u1EA9n tu\u1EA7n t\u1EF1 h\xF3a d\u1EEF li\u1EC7u th\xE2\
  n thi\u1EC7n v\u1EDBi con ng\u01B0\u1EDDi cho t\u1EA5t c\u1EA3 c\xE1c ng\xF4n ng\u1EEF\
  \ l\u1EADp tr\xECnh. C\xE1c l\u1EADp tr\xECnh\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Làm thế nào và Tại sao?
YAML không phải là ngôn ngữ đánh dấu. Nó là một tiêu chuẩn tuần tự hóa dữ liệu thân thiện với con người cho tất cả các ngôn ngữ lập trình. Các lập trình viên sử dụng nó cho các tập tin cấu hình, trao đổi dữ liệu giữa các ngôn ngữ, và nó dễ hiểu so với XML hoặc JSON.

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

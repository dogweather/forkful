---
title:                "Làm việc với YAML"
date:                  2024-01-28T22:11:36.700769-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

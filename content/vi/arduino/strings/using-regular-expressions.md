---
title:                "Sử dụng biểu thức chính quy"
aliases:
- /vi/arduino/using-regular-expressions.md
date:                  2024-01-28T22:09:20.243579-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Biểu thức chính quy (regex) cho phép bạn tìm kiếm văn bản theo mô hình—nghĩ đến là phiên bản nâng cao của wild card. Lập trình viên sử dụng chúng để xác thực nhập liệu, tìm kiếm chuỗi, và trích xuất dữ liệu một cách hiệu quả.

## Làm thế nào:

Arduino không hỗ trợ regex được tích hợp sẵn, nhưng bạn có thể mô phỏng kiểm tra mô hình đơn giản. Đối với các vấn đề nâng cao hơn, hãy cân nhắc sử dụng thư viện regex như `Regexp`.

```Arduino
#include <Regexp.h>

void setup() {
  Serial.begin(9600);
  
  MatchState ms;
  char result;
  
  ms.Target ("Hello World!");
  result = ms.Match ("(World)");

  if (result > 0) {
    char captured[10]; // Đảm bảo rằng đủ lớn để chứa trận đấu của bạn
    ms.GetCapture (captured, 0);
    Serial.print("Tìm thấy trận đấu: ");
    Serial.println(captured);
  } else {
    Serial.println("Không tìm thấy trận đấu nào.");
  }
}

void loop() {
  // Không có gì để làm ở đây.
}
```

Kết quả Mẫu:
```
Tìm thấy trận đấu: World
```

## Sâu hơn

Regex xuất phát từ khoa học máy tính lý thuyết và đã tồn tại từ những năm 1950. Perl và các ngôn ngữ khác có triển khai regex mạnh mẽ, nhưng trên Arduino, nguồn lực bị giới hạn, do đó không có hỗ trợ nội bộ. Thư viện như `Regexp` là bạn của bạn—chúng giúp giảm bớt một số gánh nặng, nhưng nhớ rằng chúng có thể nặng nề đối với các microcontroller nhỏ hơn.

## Xem Thêm

Kiểm tra những cái này để có thêm thông tin:

- Thư viện `Regexp` của Arduino: [https://www.arduino.cc/reference/en/libraries/regexp/](https://www.arduino.cc/reference/en/libraries/regexp/)
- GitHub repo của thư viện `Regexp`: [https://github.com/nickgammon/Regexp](https://github.com/nickgammon/Regexp)
- Trình kiểm tra regex trực tuyến (để soạn thảo regex của bạn trước khi triển khai): [https://regexr.com/](https://regexr.com/)

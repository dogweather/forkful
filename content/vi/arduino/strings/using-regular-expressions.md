---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:20.243579-07:00
description: "L\xE0m th\u1EBF n\xE0o: Arduino kh\xF4ng h\u1ED7 tr\u1EE3 regex \u0111\
  \u01B0\u1EE3c t\xEDch h\u1EE3p s\u1EB5n, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 m\xF4\
  \ ph\u1ECFng ki\u1EC3m tra m\xF4 h\xECnh \u0111\u01A1n gi\u1EA3n. \u0110\u1ED1i\
  \ v\u1EDBi c\xE1c v\u1EA5n \u0111\u1EC1 n\xE2ng cao h\u01A1n, h\xE3y c\xE2n\u2026"
lastmod: '2024-03-13T22:44:36.977535-06:00'
model: gpt-4-0125-preview
summary: "Arduino kh\xF4ng h\u1ED7 tr\u1EE3 regex \u0111\u01B0\u1EE3c t\xEDch h\u1EE3\
  p s\u1EB5n, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 m\xF4 ph\u1ECFng ki\u1EC3m tra m\xF4\
  \ h\xECnh \u0111\u01A1n gi\u1EA3n."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

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

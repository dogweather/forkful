---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:20.243579-07:00
description: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) cho ph\xE9p b\u1EA1n t\xEC\
  m ki\u1EBFm v\u0103n b\u1EA3n theo m\xF4 h\xECnh\u2014ngh\u0129 \u0111\u1EBFn l\xE0\
  \ phi\xEAn b\u1EA3n n\xE2ng cao c\u1EE7a wild card. L\u1EADp tr\xECnh vi\xEAn s\u1EED\
  \ d\u1EE5ng ch\xFAng \u0111\u1EC3 x\xE1c\u2026"
lastmod: '2024-02-25T18:49:35.324397-07:00'
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) cho ph\xE9p b\u1EA1n t\xECm ki\u1EBF\
  m v\u0103n b\u1EA3n theo m\xF4 h\xECnh\u2014ngh\u0129 \u0111\u1EBFn l\xE0 phi\xEA\
  n b\u1EA3n n\xE2ng cao c\u1EE7a wild card. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5\
  ng ch\xFAng \u0111\u1EC3 x\xE1c\u2026"
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
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

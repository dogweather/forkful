---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:23.836019-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Dart cung c\u1EA5p c\xE1c ph\u01B0\u01A1\
  ng th\u1EE9c b\u1EA3n \u0111\u1ECBa trong ki\u1EC3u `num` c\u1EE7a n\xF3 cho c\xE1\
  c thao t\xE1c l\xE0m tr\xF2n. T\u1EA1i \u0111\xE2y, ch\xFAng ta s\u1EBD kh\xE1m\
  \ ph\xE1 c\xE1c ph\u01B0\u01A1ng th\u1EE9c nh\u01B0\u2026"
lastmod: '2024-03-13T22:44:36.255987-06:00'
model: gpt-4-0125-preview
summary: "Dart cung c\u1EA5p c\xE1c ph\u01B0\u01A1ng th\u1EE9c b\u1EA3n \u0111\u1ECB\
  a trong ki\u1EC3u `num` c\u1EE7a n\xF3 cho c\xE1c thao t\xE1c l\xE0m tr\xF2n."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

## Cách thực hiện:
Dart cung cấp các phương thức bản địa trong kiểu `num` của nó cho các thao tác làm tròn. Tại đây, chúng ta sẽ khám phá các phương thức như `round()`, `floor()`, `ceil()`, và cách làm tròn đến một số lượng chữ số thập phân nhất định.

### Làm tròn đến số nguyên gần nhất:
```dart
var number = 3.56;
print(number.round()); // Đầu ra: 4
```

### Làm tròn xuống:
```dart
print(number.floor()); // Đầu ra: 3
```

### Làm tròn lên:
```dart
print(number.ceil()); // Đầu ra: 4
```

### Làm tròn đến một số lượng chữ số thập phân nhất định:
Để làm tròn đến một số lượng chữ số thập phân nhất định, chúng ta có thể sử dụng phương thức `toStringAsFixed()`, phương thức này trả về một chuỗi, hoặc sử dụng sự kết hợp của `pow` từ `dart:math` để có kết quả số.

```dart
import 'dart:math';

var number = 3.56789;
String roundedString = number.toStringAsFixed(2); // Dành cho mục đích hiển thị
print(roundedString); // Đầu ra: 3.57

double roundedNumber = double.parse(roundedString);
print(roundedNumber); // Đầu ra: 3.57

// Hoặc, cho một kết quả số:
double roundedToDecimal = (number * pow(10, 2)).round().toDouble() / pow(10, 2);
print(roundedToDecimal); // Đầu ra: 3.57
```

Mặc dù thư viện cốt lõi của Dart đáp ứng hầu hết các nhu cầu làm tròn một cách hiệu quả, đối với những thao tác toán học phức tạp hơn hoặc các yêu cầu làm tròn chính xác, các thư viện như `decimal` có thể hữu ích. Thư viện `decimal` cung cấp một cách dễ dàng để làm việc với số thập phân mà không mất độ chính xác, điều này đặc biệt hữu ích cho các tính toán tài chính, nhưng đối với các phương pháp làm tròn đơn giản như đã cho thấy, chức năng cốt lõi của Dart thường là đủ.

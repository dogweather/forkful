---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:25.492734-06:00
description: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\u0E41\u0E1A\u0E1A\u0E14\u0E35\
  \u0E1A\u0E31\u0E01\u0E43\u0E19 Dart \u0E40\u0E1B\u0E47\u0E19\u0E01\u0E32\u0E23\u0E41\
  \u0E2A\u0E14\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E2D\u0E01\u0E21\u0E32\
  \u0E22\u0E31\u0E07\u0E04\u0E2D\u0E19\u0E42\u0E0B\u0E25\u0E43\u0E19\u0E23\u0E30\u0E2B\
  \u0E27\u0E48\u0E32\u0E07\u0E17\u0E35\u0E48\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E17\u0E33\u0E07\u0E32\u0E19 \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E19\u0E31\
  \u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E15\u0E34\
  \u0E14\u0E15\u0E32\u0E21\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E02\u0E2D\
  \u0E07\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u2026"
lastmod: '2024-03-17T21:57:55.899467-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\u0E41\u0E1A\u0E1A\u0E14\u0E35\
  \u0E1A\u0E31\u0E01\u0E43\u0E19 Dart \u0E40\u0E1B\u0E47\u0E19\u0E01\u0E32\u0E23\u0E41\
  \u0E2A\u0E14\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E2D\u0E01\u0E21\u0E32\
  \u0E22\u0E31\u0E07\u0E04\u0E2D\u0E19\u0E42\u0E0B\u0E25\u0E43\u0E19\u0E23\u0E30\u0E2B\
  \u0E27\u0E48\u0E32\u0E07\u0E17\u0E35\u0E48\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E17\u0E33\u0E07\u0E32\u0E19 \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E19\u0E31\
  \u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E15\u0E34\
  \u0E14\u0E15\u0E32\u0E21\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E02\u0E2D\
  \u0E07\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u2026"
title: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E49\u0E14"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การพิมพ์ข้อความแสดงผลแบบดีบักใน Dart เป็นการแสดงข้อมูลออกมายังคอนโซลในระหว่างที่โปรแกรมทำงาน ช่วยให้นักพัฒนาสามารถติดตามการทำงานของโปรแกรม สืบสวนสถานะของตัวแปร หรือหาสาเหตุของข้อผิดพลาด โปรแกรมเมอร์มักใช้มันในการตรวจสอบและยืนยันว่าโค้ดที่เขียนทำงานได้ตามที่คาดหวัง ส่งเสริมกระบวนการพัฒนาที่ราบรื่นและมีประสิทธิภาพมากขึ้น

## วิธีการ:

ใน Dart คุณสามารถพิมพ์ข้อความแสดงผลแบบดีบักโดยใช้ฟังก์ชัน `print()` นี่คือวิธีการแสดงข้อความง่ายๆ และค่าของตัวแปร:

```dart
void main() {
  String greeting = "สวัสดี, Dart!";
  print(greeting); // แสดง: สวัสดี, Dart!

  int number = 42;
  print('เลขที่เป็น $number.'); // แสดง: เลขที่เป็น 42.
}
```

สำหรับข้อมูลโครงสร้างเช่นลิสต์หรืออ็อบเจกต์, วิธีการ `toString()` ของ Dart อาจไม่ให้รายละเอียดเพียงพอ ในกรณีเหล่านั้น คุณสามารถใช้ฟังก์ชัน `jsonEncode` จากห้องสมุด `dart:convert` ของ Dart เพื่อแปลงข้อมูลเป็นสตริง JSON เพื่อให้ผลลัพธ์ที่อ่านง่ายขึ้น:

```dart
import 'dart:convert';

void main() {
  var user = {
    'name': 'John Doe',
    'age': 30,
    'emails': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(user));
  // แสดง: {"name":"John Doe","age":30,"emails":["john.doe@example.com","john@example.com"]}
}
```

เมื่อต้องการความสามารถในการดีบักที่ซับซ้อนมากขึ้น เช่น การบันทึกกับระดับความสำคัญที่ต่างกัน (info, warning, error) คุณสามารถใช้ห้องสมุดของบุคคลที่สามเช่น `logger` นี่คือวิธีใช้:

1. เพิ่ม `logger` ไปยัง `pubspec.yaml` ของคุณ:

```yaml
dependencies:
  logger: ^1.0.0
```

2. ใช้ `logger` ในโค้ด Dart ของคุณ:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("นี่คือข้อความดีบัก");
  logger.w("นี่คือข้อความเตือน");
  logger.e("นี่คือข้อความแสดงข้อผิดพลาด");
}
```

ผลลัพธ์จะแสดงข้อมูลที่มีประโยชน์มากขึ้น โดยแสดงระดับของข้อความและข้อความเอง ทำให้ง่ายขึ้นในการแยกแยะระหว่างประเภทของข้อความบันทึกต่างๆ

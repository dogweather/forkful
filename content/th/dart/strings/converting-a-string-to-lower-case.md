---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:51.198903-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E43\u0E19 Dart, \u0E04\u0E38\
  \u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E25\u0E47\u0E01\u0E44\u0E14\u0E49\u0E42\u0E14\
  \u0E22\u0E43\u0E0A\u0E49\u0E40\u0E21\u0E18\u0E2D\u0E14 `toLowerCase()` \u0E17\u0E35\
  \u0E48 `String` class \u0E43\u0E2B\u0E49\u0E21\u0E32\u2026"
lastmod: '2024-03-17T21:57:55.882429-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Dart, \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\
  \u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\
  \u0E47\u0E19\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E25\
  \u0E47\u0E01\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E40\u0E21\u0E18\
  \u0E2D\u0E14 `toLowerCase()` \u0E17\u0E35\u0E48 `String` class \u0E43\u0E2B\u0E49\
  \u0E21\u0E32 \u0E27\u0E34\u0E18\u0E35\u0E19\u0E35\u0E49\u0E08\u0E30\u0E2A\u0E48\u0E07\
  \u0E04\u0E37\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E2B\u0E21\u0E48\u0E17\u0E35\
  \u0E48\u0E21\u0E35\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E15\u0E31\u0E27\u0E1E\u0E34\
  \u0E21\u0E1E\u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E17\u0E31\u0E49\u0E07\u0E2B\u0E21\u0E14\
  \u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\
  \u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E25\u0E47\u0E01 \u0E25\u0E2D\u0E07\u0E14\u0E39\
  \u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E19\u0E35\u0E49\u0E14\u0E49\u0E27\u0E22\
  \u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E46."
title: "\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E47\u0E01"
weight: 4
---

## วิธีทำ:
ใน Dart, คุณสามารถเปลี่ยนสตริงเป็นอักษรพิมพ์เล็กได้โดยใช้เมธอด `toLowerCase()` ที่ `String` class ให้มา วิธีนี้จะส่งคืนสตริงใหม่ที่มีอักขระตัวพิมพ์ใหญ่ทั้งหมดเปลี่ยนเป็นตัวพิมพ์เล็ก ลองดูวิธีการนี้ด้วยตัวอย่างง่ายๆ:

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();

  print(lowerCaseString);  // ผลลัพธ์: hello, world!
}
```

Dart ไม่ต้องการไลบรารีภายนอกสำหรับการจัดการสตริงพื้นฐาน, รวมถึงการเปลี่ยนเป็นตัวพิมพ์เล็ก เนื่องจาก `String` class ในไลบรารีมาตรฐานค่อนข้างครอบคลุม อย่างไรก็ตาม, สำหรับการจัดการที่ซับซ้อนมากขึ้นซึ่งเกี่ยวข้องกับกฎที่เจาะจงต่อท้องถิ่น, คุณอาจพิจารณา `intl` package, ซึ่งให้บริการสากลภาษาและการจำแนกท้องถิ่นรวมถึงการเปลี่ยนรูปแบบตัวอักษรบนพื้นฐานของท้องถิ่น:

เพื่อใช้ `intl`, เพิ่มมันลงในไฟล์ `pubspec.yaml` ของคุณ:

```yaml
dependencies:
  intl: ^0.17.0
```

จากนั้น, คุณสามารถใช้เมธอด `toLocaleLowerCase()` เพื่อเปลี่ยนสตริงเป็นตัวพิมพ์เล็กตามท้องถิ่นที่เฉพาะเจาะจง:

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // ท้องถิ่นเตอร์กี
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // ผลลัพธ์: istanbul
  
  // ท้องถิ่นเริ่มต้น (en)
  print(originalString.toLowerCase()); // ผลลัพธ์: i̇stanbul
}
```

ในตัวอย่างนี้, สังเกตว่าท้องถิ่นเตอร์กีจัดการกับ ตัว 'i' ที่ไม่มีจุดได้อย่างถูกต้อง แสดงให้เห็นถึงความสำคัญของการแปลงโดยระวังนำพาท้องถิ่นในแอปพลิเคชั่นที่ใช้หลายภาษา.

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:39.708382-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01 substring \u0E04\u0E37\u0E2D\u0E01\
  \u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E1A\u0E32\u0E07\
  \u0E2A\u0E48\u0E27\u0E19\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E15\u0E32\
  \u0E21\u0E15\u0E33\u0E41\u0E2B\u0E19\u0E48\u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E40\u0E09\u0E1E\u0E32\u0E30 \u0E19\u0E31\u0E01\u0E1E\u0E31\
  \u0E12\u0E19\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E40\u0E0A\u0E48\u0E19\u0E01\u0E32\u0E23\
  \u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E1B\u0E49\
  \u0E2D\u0E19\u0E42\u0E14\u0E22\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49, \u0E01\u0E32\
  \u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
lastmod: '2024-03-17T21:57:55.884453-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01 substring \u0E04\u0E37\u0E2D\u0E01\u0E32\
  \u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E1A\u0E32\u0E07\u0E2A\
  \u0E48\u0E27\u0E19\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E15\u0E32\u0E21\
  \u0E15\u0E33\u0E41\u0E2B\u0E19\u0E48\u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E23\u0E39\u0E1B\
  \u0E41\u0E1A\u0E1A\u0E40\u0E09\u0E1E\u0E32\u0E30 \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\
  \u0E19\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E40\u0E0A\u0E48\u0E19\u0E01\u0E32\u0E23\u0E41\
  \u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E1B\u0E49\u0E2D\
  \u0E19\u0E42\u0E14\u0E22\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49, \u0E01\u0E32\u0E23\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
---

{{< edit_this_page >}}

## คืออะไร & ทำไม?
การแยก substring คือการดึงข้อมูลบางส่วนของสตริงตามตำแหน่งหรือรูปแบบเฉพาะ นักพัฒนาทำเช่นนี้สำหรับงานเช่นการแยกข้อมูลที่ป้อนโดยผู้ใช้, การจัดการข้อมูล, หรือการดึงข้อมูลที่เกี่ยวข้องจากแหล่งข้อความที่ใหญ่กว่า

## วิธีการ:
ใน Dart, คุณสามารถใช้วิธีการต่างๆในการแยกสตริงย่อย เช่น `substring()`, `split()`, และ regular expressions แต่ละวิธีทำงานต่างกันและมอบความยืดหยุ่นในการจัดการสตริง

### การใช้ `substring()`:
วิธีการ `substring()` เป็นวิธีง่ายๆ คุณกำหนดดัชนีเริ่มต้น (และทางเลือก, ดัชนีสิ้นสุด) เพื่อตัดส่วนของสตริง

```dart
void main() {
  String example = "Hello, World!";
  String result = example.substring(7, 12);
  print(result); // ผลลัพธ์: World
}
```

### การใช้ `split()`:
แยกสตริงเป็นรายการของสตริงย่อยตามรูปแบบ (เช่น ช่องว่างหรือจุลภาค) จากนั้นเข้าถึงสตริงย่อยด้วยดัชนี

```dart
void main() {
  String example = "Dart is fun";
  List<String> parts = example.split(' ');
  String result = parts[1]; // เข้าถึงด้วยดัชนี
  print(result); // ผลลัพธ์: is
}
```

### การใช้ Regular Expressions:
สำหรับรูปแบบที่ซับซ้อน, คลาส `RegExp` ของ Dart มีประสิทธิภาพอย่างมาก ใช้มันเพื่อจับคู่รูปแบบและแยกสตริงย่อยออกมา

```dart
void main() {
  String example = "Email: example@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String email = regExp.stringMatch(example)!;
  print(email); // ผลลัพธ์: example@mail.com
}
```

### ไลบรารีของบุคคลที่สาม:
แม้ว่าไลบรารีมาตรฐานของ Dart จะมีประสิทธิภาพมากพอ, คุณอาจพบสถานการณ์ที่ไลบรารีของบุคคลที่สามอาจทำให้งานของคุณง่ายขึ้น ตัวเลือกยอดนิยมสำหรับการจัดการสตริงและการจับคู่รูปแบบไม่ได้รับการสนับสนุนโดยเฉพาะที่นี่เนื่องจากความสามารถในตัวของ Dart มักจะเพียงพอ อย่างไรก็ตาม, ตรวจสอบเสมอ [pub.dev](https://pub.dev) สำหรับไลบรารีใดๆ ที่อาจเหมาะกับความต้องการเฉพาะของคุณได้ดีกว่า

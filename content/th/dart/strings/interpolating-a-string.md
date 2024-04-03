---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:28.202576-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E02\u0E49\u0E2D\u0E04\u0E27\
  \u0E32\u0E21\u0E41\u0E1A\u0E1A\u0E21\u0E35\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\u0E40\
  \u0E02\u0E49\u0E32\u0E44\u0E1B (String interpolation)\u2026"
lastmod: '2024-03-17T21:57:55.881642-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E02\u0E49\u0E2D\u0E04\u0E27\
  \u0E32\u0E21\u0E41\u0E1A\u0E1A\u0E21\u0E35\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\u0E40\
  \u0E02\u0E49\u0E32\u0E44\u0E1B (String interpolation) \u0E04\u0E37\u0E2D\u0E01\u0E23\
  \u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E43\u0E2A\u0E48\u0E04\
  \u0E48\u0E32\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\
  \u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\u0E42\
  \u0E14\u0E22\u0E21\u0E31\u0E01\u0E43\u0E0A\u0E49\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E17\u0E35\u0E48\u0E21\u0E35\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\
  \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\u0E01\
  \u0E32\u0E23\u0E40\u0E0A\u0E37\u0E48\u0E2D\u0E21\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E17\u0E35\u0E48\u0E22\u0E38\u0E48\u0E07\u0E22\u0E32\u0E01 \u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\
  \u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E04\u0E49\u0E14\
  \u0E17\u0E35\u0E48\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E41\u0E25\u0E30\u0E2D\u0E48\u0E32\
  \u0E19\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\u0E49\u0E19\u0E41\u0E25\u0E30\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E1B\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E19\u0E02\u0E49\u0E2D\u0E1C\
  \u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E17\u0E35\u0E48\u0E21\u0E31\u0E01\u0E40\u0E01\
  \u0E34\u0E14\u0E02\u0E36\u0E49\u0E19\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E0A\u0E37\
  \u0E48\u0E2D\u0E21\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E17\u0E35\u0E48\
  \u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19."
title: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E04\u0E48\u0E32\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 8
---

## อะไรและทำไม?

การแทรกข้อความแบบมีตัวแปรเข้าไป (String interpolation) คือกระบวนการที่ใส่ค่าตัวแปรเข้าไปในสตริงโดยตรงโดยมักใช้สำหรับการสร้างข้อความที่มีความหมายโดยไม่ต้องใช้การเชื่อมต่อสตริงที่ยุ่งยาก โปรแกรมเมอร์ทำเช่นนี้เพื่อโค้ดที่สะอาดและอ่านง่ายขึ้นและเพื่อป้องกันข้อผิดพลาดที่มักเกิดขึ้นในการเชื่อมต่อสตริงที่ซับซ้อน

## วิธีการ:

ใน Dart, การแทรกข้อความแบบมีตัวแปรนั้นง่ายดายโดยใช้สัญลักษณ์ `$` เพื่อแทรกคำสั่งโดยตรงในตัวอักษรของสตริง:

```dart
void main() {
  String name = 'Dart';
  int year = 2023;
  // การแทรกตัวแปรแบบง่าย
  print('Learning $name in $year!');
  // ผลลัพธ์: Learning Dart in 2023!
  
  // การแทรกคำสั่ง
  print('In two years, it will be ${year + 2}.');
  // ผลลัพธ์: In two years, it will be 2025.
}
```

ในกรณีที่คุณมีคำสั่งที่ซับซ้อนขึ้นหรือต้องการทำการคำนวณภายในสตริงเอง ให้ครอบคำสั่งด้วย `${}`. Dart ไม่มีไลบรารี่ของบุคคลที่สามที่นิยมใช้เฉพาะสำหรับการแทรกข้อความแบบมีตัวแปรเนื่องจากตัวภาษามีความสามารถอยู่แล้วที่จะจัดการกับสถานการณ์ที่หลากหลายและซับซ้อนได้เอง

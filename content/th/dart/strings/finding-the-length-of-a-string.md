---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:54.740980-06:00
description: "\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\
  \u0E02\u0E2D\u0E07 String \u0E43\u0E19 Dart \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\
  \u0E1E\u0E34\u0E08\u0E32\u0E23\u0E13\u0E32\u0E08\u0E33\u0E19\u0E27\u0E19\u0E2B\u0E19\
  \u0E48\u0E27\u0E22\u0E02\u0E2D\u0E07\u0E23\u0E2B\u0E31\u0E2A (\u0E42\u0E14\u0E22\
  \u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E41\u0E25\u0E49\u0E27\u0E04\u0E37\u0E2D\
  \u0E08\u0E33\u0E19\u0E27\u0E19\u0E02\u0E2D\u0E07\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\
  \u0E29\u0E23\u0E2B\u0E32\u0E01\u0E04\u0E34\u0E14\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\
  \u0E48\u0E32\u0E22\u0E46) \u0E43\u0E19 String \u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\
  \u0E19\u0E14\u2026"
lastmod: '2024-03-17T21:57:55.886277-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\
  \u0E02\u0E2D\u0E07 String \u0E43\u0E19 Dart \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\
  \u0E1E\u0E34\u0E08\u0E32\u0E23\u0E13\u0E32\u0E08\u0E33\u0E19\u0E27\u0E19\u0E2B\u0E19\
  \u0E48\u0E27\u0E22\u0E02\u0E2D\u0E07\u0E23\u0E2B\u0E31\u0E2A (\u0E42\u0E14\u0E22\
  \u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E41\u0E25\u0E49\u0E27\u0E04\u0E37\u0E2D\
  \u0E08\u0E33\u0E19\u0E27\u0E19\u0E02\u0E2D\u0E07\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\
  \u0E29\u0E23\u0E2B\u0E32\u0E01\u0E04\u0E34\u0E14\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\
  \u0E48\u0E32\u0E22\u0E46) \u0E43\u0E19 String \u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\
  \u0E19\u0E14\u2026"
title: "\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\u0E02\
  \u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การหาความยาวของ String ใน Dart คือการพิจารณาจำนวนหน่วยของรหัส (โดยพื้นฐานแล้วคือจำนวนของตัวอักษรหากคิดอย่างง่ายๆ) ใน String ที่กำหนด โปรแกรมเมอร์ทำเช่นนี้เพื่อจัดการกับสตริงได้อย่างแม่นยำยิ่งขึ้น เช่น การตรวจสอบความถูกต้องของข้อมูลที่ป้อน เพื่อตัดความยาวข้อความที่แสดง หรือประมวลผลรูปแบบข้อมูลที่ความยาวมีความสำคัญ (เช่น โปรโตคอลที่มีข้อความที่กำหนดความยาวไว้ล่วงหน้า)

## วิธีการ:
Dart ทำให้การได้รับความยาวของสตริงเป็นเรื่องง่ายด้วยการใช้คุณสมบัติ `length` นี่คือตัวอย่างพื้นฐาน:

```dart
void main() {
  String myString = "Hello, Dart!";
  print("ความยาวของ '\(myString)' คือ: \(myString.length)");
  // ผลลัพธ์: ความยาวของ 'Hello, Dart!' คือ: 12
}
```
คุณสมบัตินี้นับจำนวนหน่วยรหัส UTF-16 ในสตริง ซึ่งตรงกับความยาวของสตริงสำหรับกรณีการใช้งานทั่วไปส่วนใหญ่

สำหรับการประมวลผลข้อความที่ละเอียดยิ่งขึ้น โดยเฉพาะที่เกี่ยวข้องกับตัวอักษร Unicode ที่อยู่นอกเหนือจากพื้นที่ Multilingual พื้นฐาน (BMP), พิจารณาใช้แพ็คเกจ `characters` สำหรับการนับกลุ่ม grapheme ซึ่งเป็นตัวแทนคาแรคเตอร์ที่ผู้ใช้รับรู้ได้อย่างแม่นยำยิ่งขึ้น

ขั้นแรก เพิ่ม `characters` ลงใน `pubspec.yaml` ของคุณ:

```yaml
dependencies:
  characters: ^1.2.0
```

จากนั้น ใช้มันดังนี้:

```dart
import 'package:characters/characters.dart';

void main() {
  String myEmojiString = "👨‍👩‍👧‍👦 family";
  print("ความยาวของ '\(myEmojiString)' คือ: \(myEmojiString.characters.length)");
  // ผลลัพธ์: ความยาวของ '👨‍👩‍👧‍👦 family' คือ: 8
}
```

ในตัวอย่างนี้, `myEmojiString.characters.length` ให้ความยาวในเงื่อนไขของกลุ่ม grapheme Unicode ซึ่งเป็นการแทนค่าที่แม่นยำยิงขึ้นสำหรับสตริงที่ประกอบด้วยตัวละครที่ซับซ้อน เช่น emojis หรือเครื่องหมายอักขระที่รวมกัน

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:17.894578-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Dart \u0E21\u0E2D\u0E1A\u0E27\
  \u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E07\u0E48\u0E32\u0E22\u0E46 \u0E43\u0E19\u0E01\
  \u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\u0E32\u0E28\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E42\u0E14\u0E22\u0E43\
  \u0E0A\u0E49\u0E40\u0E21\u0E18\u0E2D\u0E14\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E17\u0E35\
  \u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\u0E15\u0E31\u0E27\u0E42\u0E14\
  \u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\
  \u0E23\u0E32\u0E23\u0E35\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01."
lastmod: '2024-04-05T21:54:01.361399-06:00'
model: gpt-4-0125-preview
summary: "Dart \u0E21\u0E2D\u0E1A\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E07\u0E48\
  \u0E32\u0E22\u0E46 \u0E43\u0E19\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E0D\u0E1B\
  \u0E23\u0E30\u0E01\u0E32\u0E28\u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E40\u0E21\u0E18\u0E2D\u0E14\u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E43\
  \u0E19\u0E15\u0E31\u0E27\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\
  \u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E20\u0E32\u0E22\u0E19\
  \u0E2D\u0E01."
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\u0E32\u0E28\u0E2D\u0E2D\
  \u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 9
---

## วิธีการ:
Dart มอบวิธีการง่ายๆ ในการลบอัญประกาศออกจากสตริงโดยใช้เมธอดสตริงที่มีอยู่ในตัวโดยไม่ต้องใช้ไลบรารีภายนอก

### ตัวอย่างที่ 1: การใช้ `replaceFirst` และ `replaceAll`
หากคุณกำลังจัดการกับสตริงที่เริ่มต้นและจบด้วยอัญประกาศ คุณสามารถใช้เมธอด `replaceFirst` และ `replaceAll` เพื่อลบออกได้

```dart
String quotedString = '"Hello, World!"';
String singleQuotedString = '\'Dart Programming\'';

// ลบอัญประกาศคู่
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // ผลลัพธ์: Hello, World!

// ลบอัญประกาศเดี่ยว
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // ผลลัพธ์: Dart Programming
```

### ตัวอย่างที่ 2: การใช้ `substring`
เมธอดนี้มีประโยชน์เมื่อคุณมั่นใจว่าอัญประกาศอยู่ที่ต้นสุดและปลายสุดของสตริง

```dart
String quotedString = '"Flutter Development"';
// ตรวจสอบว่ามีอัญประกาศที่ต้นและปลายก่อนที่จะลบเพื่อหลีกเลี่ยงข้อผิดพลาด
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // ผลลัพธ์: Flutter Development
```

### ตัวอย่างที่ 3: การสร้างเมธอดส่วนขยายที่กำหนดเอง
สำหรับการใช้งานที่มีความยืดหยุ่นมากขึ้น โดยเฉพาะหากโปรเจคของคุณเกี่ยวข้องกับการลบอัญประกาศอย่างบ่อยครั้ง พิจารณาสร้างเมธอดส่วนขยายที่กำหนดเองบน `String`

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"This is Dart"';
  String singleQuoted = '\'This is awesome\'';
  print(doubleQuoted.unquote()); // ผลลัพธ์: This is Dart
  print(singleQuoted.unquote()); // ผลลัพธ์: This is awesome
}
```

วิธีการเหล่านี้ควรช่วยคุณลบอัญประกาศออกจากสตริงใน Dart ได้อย่างมีประสิทธิภาพ ทำให้คุณปรับปรุงกระบวนการประมวลผลและเตรียมข้อมูลได้ดียิ่งขึ้น

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:30.490895-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Dart \u0E43\u0E2B\u0E49\u0E1A\
  \u0E23\u0E34\u0E01\u0E32\u0E23\u0E04\u0E25\u0E32\u0E2A `DateTime` \u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E27\u0E31\
  \u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32 \u0E41\u0E25\u0E30\
  \u0E41\u0E1E\u0E47\u0E04\u0E40\u0E01\u0E08 `intl` \u0E2A\u0E33\u0E2B\u0E23\u0E31\
  \u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A \u0E01\
  \u0E48\u0E2D\u0E19\u0E2D\u0E37\u0E48\u0E19, \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\
  \u0E1A\u0E27\u0E48\u0E32\u0E04\u0E38\u0E13\u0E21\u0E35\u0E41\u0E1E\u0E47\u0E04\u0E40\
  \u0E01\u0E08 `intl`\u2026"
lastmod: '2024-03-17T21:57:55.910543-06:00'
model: gpt-4-0125-preview
summary: "Dart \u0E43\u0E2B\u0E49\u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E04\u0E25\u0E32\
  \u0E2A `DateTime` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\
  \u0E14\u0E01\u0E32\u0E23\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\
  \u0E27\u0E25\u0E32 \u0E41\u0E25\u0E30\u0E41\u0E1E\u0E47\u0E04\u0E40\u0E01\u0E08\
  \ `intl` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\
  \u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A \u0E01\u0E48\u0E2D\u0E19\u0E2D\u0E37\u0E48\u0E19\
  , \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E04\u0E38\u0E13\
  \u0E21\u0E35\u0E41\u0E1E\u0E47\u0E04\u0E40\u0E01\u0E08 `intl` \u0E42\u0E14\u0E22\
  \u0E01\u0E32\u0E23\u0E40\u0E1E\u0E34\u0E48\u0E21 `intl."
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 28
---

## วิธีการ:
Dart ให้บริการคลาส `DateTime` สำหรับการจัดการวันที่และเวลา และแพ็คเกจ `intl` สำหรับการจัดรูปแบบ ก่อนอื่น, ตรวจสอบว่าคุณมีแพ็คเกจ `intl` โดยการเพิ่ม `intl: ^0.17.0` (หรือเวอร์ชันล่าสุด) ลงในไฟล์ `pubspec.yaml` ของคุณ

### การใช้ห้องสมุดมาตรฐานของ Dart
```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // ผลลัพธ์: 2023-4-12 (ตัวอย่างเช่น, นี่ขึ้นอยู่กับวันที่ปัจจุบัน)
```

ตัวอย่างนี้แสดงการสร้างสตริงโดยตรงจากคุณสมบัติของ `DateTime`

### การใช้แพ็คเกจ `intl`
ก่อนอื่น, นำเข้าแพ็คเกจ:

```dart
import 'package:intl/intl.dart';
```

จากนั้น, จัดรูปแบบวันที่:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // ผลลัพธ์: 2023-04-12
```

แพ็คเกจ `intl` ช่วยให้การจัดรูปแบบที่ซับซ้อนได้ง่ายขึ้น รวมถึงรูปแบบที่เฉพาะตามสถานที่:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // ผลลัพธ์: April 12, 2023
```

ตัวอย่างเหล่านี้แสดงวิธีการที่ง่ายแต่ทรงพลังในการแปลงและจัดรูปแบบวันที่เป็นสตริงใน Dart ไม่ว่าจะใช้ฟังก์ชันหลักของ Dart หรือใช้แพ็คเกจ `intl` เพื่อตัวเลือกการจัดรูปแบบที่ซับซ้อนมากขึ้น

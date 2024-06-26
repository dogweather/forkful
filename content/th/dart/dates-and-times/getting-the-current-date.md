---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:12.574553-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E44\u0E25\u0E1A\u0E23\
  \u0E32\u0E23\u0E35\u0E2B\u0E25\u0E31\u0E01\u0E02\u0E2D\u0E07 Dart \u0E43\u0E2B\u0E49\
  \u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E1B\u0E31\u0E08\u0E08\u0E38\
  \u0E1A\u0E31\u0E19\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\
  \u0E22\u0E14\u0E32\u0E22\u0E1C\u0E48\u0E32\u0E19\u0E04\u0E25\u0E32\u0E2A `DateTime`\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E23\
  \u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\
  \u0E31\u0E19."
lastmod: '2024-03-17T21:57:55.909491-06:00'
model: gpt-4-0125-preview
summary: "\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E2B\u0E25\u0E31\u0E01\u0E02\
  \u0E2D\u0E07 Dart \u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E16\
  \u0E36\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\
  \u0E32\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\u0E44\u0E14\u0E49\u0E2D\u0E22\
  \u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E14\u0E32\u0E22\u0E1C\u0E48\u0E32\u0E19\
  \u0E04\u0E25\u0E32\u0E2A `DateTime` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\
  \u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\
  \u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19."
title: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\
  \u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19"
weight: 29
---

## วิธีการ:
ไลบรารีหลักของ Dart ให้การเข้าถึงวันที่และเวลาปัจจุบันได้อย่างง่ายดายผ่านคลาส `DateTime` นี่คือตัวอย่างพื้นฐานเพื่อรับวันที่ปัจจุบัน:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // ตัวอย่างผลลัพธ์: 2023-04-12 10:00:00.000
}
```

หากคุณต้องการเพียงส่วนของวันที่ (ปี, เดือน, วัน), คุณสามารถจัดรูปแบบออบเจกต์ `DateTime`:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // ตัวอย่างผลลัพธ์: 2023-04-12
}
```

Dart ไม่มีไลบรารีในตัวสำหรับการจัดรูปแบบวันที่ที่ซับซ้อนมากขึ้น, แต่คุณสามารถใช้แพ็คเกจ `intl` สำหรับวัตถุประสงค์นี้ ก่อนอื่น, เพิ่มแพ็คเกจไปยัง `pubspec.yaml` ของคุณ:

```yaml
dependencies:
  intl: ^0.17.0
```

จากนั้น, คุณสามารถจัดรูปแบบวันที่ได้อย่างง่ายดาย:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // ตัวอย่างผลลัพธ์: 2023-04-12
}
```

สำหรับตัวเลือกการจัดรูปแบบที่ซับซ้อนยิ่งขึ้น, สำรวจคลาส `DateFormat` ที่ให้บริการโดยแพ็คเกจ `intl`, ซึ่งรองรับแพทเทิร์นและสถานที่มากมาย

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:37.123728-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E43\u0E19 Dart \u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E04\u0E48\u0E32\u0E41\u0E17\
  \u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E02\u0E2D\u0E07\u0E27\u0E31\u0E19\
  \u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E40\u0E1B\u0E47\u0E19\
  \u0E2D\u0E47\u0E2D\u0E1A\u0E40\u0E08\u0E01\u0E15\u0E4C `DateTime`\u2026"
lastmod: '2024-03-17T21:57:55.908544-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E43\u0E19 Dart \u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E04\u0E48\u0E32\u0E41\u0E17\
  \u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E02\u0E2D\u0E07\u0E27\u0E31\u0E19\
  \u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E40\u0E1B\u0E47\u0E19\
  \u0E2D\u0E47\u0E2D\u0E1A\u0E40\u0E08\u0E01\u0E15\u0E4C `DateTime`\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การแยกวิเคราะห์วันที่จากสตริงใน Dart ประกอบด้วยการแปลงค่าแทนข้อความของวันที่และเวลาเป็นอ็อบเจกต์ `DateTime` การดำเนินการนี้จำเป็นสำหรับแอปพลิเคชันที่มีการจัดการกับการนัดหมาย, การวิเคราะห์ข้อมูล, หรือคุณลักษณะใด ๆ ที่ต้องการการจัดการกับวันที่ เพื่อให้แน่ใจว่าข้อมูลที่เกี่ยวข้องกับวันที่ถูกเข้าใจและประมวลผลโดยโปรแกรมอย่างถูกต้อง

## วิธีการ:
ไลบรารีหลักของ Dart ทำให้การแยกวิเคราะห์วันที่ง่ายขึ้นผ่านคลาส `DateTime` สำหรับกรณีที่ง่ายที่คุณทราบรูปแบบของสตริงวันที่, คุณสามารถใช้เมธอด `DateTime.parse()` อย่างไรก็ตาม, สำหรับสถานการณ์ที่ซับซ้อนกว่าหรือเมื่อมีการจัดการกับหลายรูปแบบ, แพ็คเกจ `intl` โดยเฉพาะคลาส `DateFormat` จึงมีค่ามาก

### การใช้งานไลบรารีหลักของ Dart:
```dart
void main() {
  // ใช้งาน DateTime.parse()
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### การใช้งานแพ็คเกจ `intl`:
เริ่มต้น, เพิ่มแพ็คเกจ `intl` ลงในไฟล์ `pubspec.yaml` ของคุณ:
```yaml
dependencies:
  intl: ^0.17.0
```
จากนั้น, นำเข้าแพ็คเกจและใช้ `DateFormat` สำหรับการแยกวิเคราะห์:
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "October 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
แพ็คเกจ `intl` นำเสนอตัวเลือกที่แข็งแกร่งสำหรับการแยกวิเคราะห์วันที่, อนุญาตให้จัดการกับรูปแบบวันที่สากลต่างๆได้อย่างราบรื่น

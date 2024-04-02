---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:16.088509-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E19 Dart \u0E19\u0E31\u0E49\u0E19\u0E40\
  \u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E41\u0E25\u0E30\u0E40\u0E23\u0E35\
  \u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E44\u0E1F\u0E25\
  \u0E4C\u0E17\u0E35\u0E48\u0E40\u0E01\u0E47\u0E1A\u0E2D\u0E22\u0E39\u0E48\u0E1A\u0E19\
  \u0E23\u0E30\u0E1A\u0E1A\u0E44\u0E1F\u0E25\u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\
  \u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E19\u0E33\u0E40\u0E02\u0E49\u0E32,\u2026"
lastmod: '2024-03-17T21:57:55.918502-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E19 Dart \u0E19\u0E31\u0E49\u0E19\u0E40\
  \u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E41\u0E25\u0E30\u0E40\u0E23\u0E35\
  \u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E44\u0E1F\u0E25\
  \u0E4C\u0E17\u0E35\u0E48\u0E40\u0E01\u0E47\u0E1A\u0E2D\u0E22\u0E39\u0E48\u0E1A\u0E19\
  \u0E23\u0E30\u0E1A\u0E1A\u0E44\u0E1F\u0E25\u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\
  \u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E19\u0E33\u0E40\u0E02\u0E49\u0E32,\u2026"
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 22
---

## อะไร & ทำไม?

การอ่านไฟล์ข้อความใน Dart นั้นเกี่ยวข้องกับการเข้าถึงและเรียกข้อมูลจากไฟล์ที่เก็บอยู่บนระบบไฟล์ โปรแกรมเมอร์ทำเช่นนี้เพื่อจัดการข้อมูลนำเข้า, การตั้งค่าคอนฟิก, หรืออ่านชุดข้อมูล, ทำให้เป็นการดำเนินการพื้นฐานสำหรับแอปพลิเคชันมากมาย ตั้งแต่สคริปต์ง่ายๆ ถึงแอปซับซ้อน

## วิธีการ:

ไลบรารีหลักของ Dart, `dart:io`, ให้ฟังก์ชั่นที่จำเป็นในการอ่านไฟล์ข้อความแบบออกจากลำดับหรือไม่ออกจากลำดับ นี่คือวิธีการทั้งสอง

**แบบออกจากลำดับ:**

```dart
import 'dart:io';

void main() {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  // อ่านไฟล์แบบออกจากลำดับ
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('Error reading file: $e');
  }
}
```

**แบบไม่ออกจากลำดับ:**

เพื่อหลีกเลี่ยงการบล็อกโปรแกรมขณะที่ไฟล์กำลังถูกอ่าน, โดยเฉพาะอย่างยิ่งสำหรับไฟล์ขนาดใหญ่หรือแอปพลิเคชันที่ตอบสนองได้ดี:

```dart
import 'dart:io';

void main() async {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('Error reading file: $e');
  }
}
```

**ตัวอย่างผลลัพธ์:**

ถ้าไฟล์ข้อความของคุณมี:

```
Hello, Dart!
```

วิธีการทั้งสองด้านบนจะแสดงผล:

```
Hello, Dart!
```

**การใช้ไลบรารีภายนอก:**

สำหรับคุณสมบัติเพิ่มเติมเช่น การดำเนินการกับไฟล์ที่ง่ายขึ้นหรือการจัดการข้อผิดพลาดที่ดีขึ้น, คุณอาจพิจารณาไลบรารีภายนอกเช่น `package:file`. อย่างไรก็ตาม, ตามการอัปเดตครั้งสุดท้ายของฉัน, การใช้แพ็คเกจ `dart:io` หลักโดยตรงตามที่แสดงข้างต้น, เป็นวิธีที่พบบ่อยที่สุดและตรงไปตรงมาที่สุดสำหรับการอ่านไฟล์ข้อความใน Dart.

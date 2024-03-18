---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:39.857301-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E19 command line \u0E43\u0E19\
  \ Dart \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\
  \u0E21\u0E2D\u0E23\u0E4C\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E19\u0E33\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E04\u0E2D\
  \u0E19\u0E42\u0E0B\u0E25\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\u0E40\
  \u0E21\u0E37\u0E48\u0E2D\u0E1B\u0E0F\u0E34\u0E1A\u0E31\u0E15\u0E34\u0E01\u0E32\u0E23\
  \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21 Dart,\u2026"
lastmod: '2024-03-17T21:57:55.916062-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E19 command line \u0E43\u0E19\
  \ Dart \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\
  \u0E21\u0E2D\u0E23\u0E4C\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E19\u0E33\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E04\u0E2D\
  \u0E19\u0E42\u0E0B\u0E25\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\u0E40\
  \u0E21\u0E37\u0E48\u0E2D\u0E1B\u0E0F\u0E34\u0E1A\u0E31\u0E15\u0E34\u0E01\u0E32\u0E23\
  \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21 Dart,\u2026"
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การอ่านอาร์กิวเมนต์บน command line ใน Dart ทำให้โปรแกรมเมอร์สามารถนำข้อมูลเข้าไปในคอนโซลได้โดยตรงเมื่อปฏิบัติการโปรแกรม Dart, เพิ่มความสามารถในการโต้ตอบและความยืดหยุ่นสำหรับกรณีการใช้งานต่างๆ เช่น สคริปต์อัตโนมัติ, เครื่องมือบน CLI, หรือการประมวลผลแบบชุด. คุณลักษณะนี้เป็นสำคัญสำหรับการสร้างแอปพลิเคชันบน command line ที่ปรับแต่งได้และใช้งานง่าย.

## วิธีการ:

Dart มีวิธีการที่ตรงไปตรงมาในการเข้าถึงอาร์กิวเมนต์บน command line ผ่านทาง `List<String> args` ในเมโทดหลัก. ด้านล่างนี้คือตัวอย่างง่ายๆ ที่แสดงวิธีการอ่านและใช้ประโยชน์จากอาร์กิวเมนต์บน command line.

```dart
// main.dart
void main(List<String> args) {
  print('อาร์กิวเมนต์ Command Line:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

เพื่อเรียกใช้โปรแกรม Dart นี้และส่งอาร์กิวเมนต์บน command line, ใช้ Dart CLI ดังนี้:

```shell
dart run main.dart สวัสดี ชาวโลก!
```

ผลลัพธ์ที่คาดหวัง:

```
อาร์กิวเมนต์ Command Line:
1: สวัสดี
2: ชาวโลก!
```

### การใช้งานห้องสมุดบุคคลที่สามยอดนิยม: `args`

ในขณะที่ความสามารถภายในตัวของ Dart สำหรับการจัดการอาร์กิวเมนต์บน command line นั้นเพียงพอสำหรับแอปพลิเคชันหลายๆ อย่างแล้ว, แพคเกจ `args` นำเสนอวิธีการที่ปรับแต่งได้เพื่อกำหนดและแยกวิเคราะห์อาร์กิวเมนต์บน command line สำหรับความต้องการที่ซับซ้อนยิ่งขึ้น.

ก่อนอื่น, เพิ่มแพคเกจ `args` ลงใน `pubspec.yaml` ของคุณ:

```yaml
dependencies:
  args: ^2.0.0
```

จากนั้น, ใช้มันในโปรแกรมของคุณดังนี้:

```dart
// การใช้ 'args' package
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('สวัสดี, ${argResults['name']}!');
  } else {
    print('ไม่มีชื่อที่ให้มา.');
  }
}
```

เรียกใช้โปรแกรมด้วยอาร์กิวเมนต์ที่มีชื่อ:

```shell
dart run main.dart --name=John
```

ผลลัพธ์ที่คาดหวัง:

```
สวัสดี, John!
```

การแนะนำนี้เกี่ยวกับการแยกวิเคราะห์อาร์กิวเมนต์บน command line, ทั้งด้วยวิธีการภายในตัวของ Dart และด้วยห้องสมุด `args`, แสดงถึงวิธีการที่ Dart สามารถจัดการกับอินพุตจากผู้ใช้ได้โดยตรงจากคอนโซล, เปิดทางไปสู่การสร้างแอปพลิเคชันบน CLI ที่มีการโต้ตอบและไดนามิกมากยิ่งขึ้น.

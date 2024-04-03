---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:36.477471-06:00
description: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19 interactive shell\
  \ (REPL - Read-Evaluate-Print Loop) \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A Dart \u0E17\
  \u0E33\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E41\
  \u0E25\u0E30\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E23\u0E31\u0E19\
  \u0E42\u0E04\u0E49\u0E14 Dart\u2026"
lastmod: '2024-03-17T21:57:55.898234-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19 interactive shell\
  \ (REPL - Read-Evaluate-Print Loop) \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A Dart \u0E17\
  \u0E33\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E41\
  \u0E25\u0E30\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E23\u0E31\u0E19\
  \u0E42\u0E04\u0E49\u0E14 Dart \u0E17\u0E35\u0E25\u0E30\u0E1A\u0E23\u0E23\u0E17\u0E31\
  \u0E14\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E04\u0E2D\u0E21\
  \u0E44\u0E1E\u0E25\u0E4C\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E17\u0E31\u0E49\
  \u0E07\u0E2B\u0E21\u0E14\u3002\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\u0E37\
  \u0E2D\u0E19\u0E35\u0E49\u0E21\u0E35\u0E04\u0E48\u0E32\u0E21\u0E32\u0E01\u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E19\u0E23\u0E39\
  \u0E49\u0E44\u0E27\u0E22\u0E32\u0E01\u0E23\u0E13\u0E4C\u0E02\u0E2D\u0E07 Dart, \u0E17\
  \u0E14\u0E25\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E2A\u0E48\u0E27\u0E19\u0E15\u0E48\u0E32\
  \u0E07\u0E46 \u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14\u0E2B\u0E23\u0E37\u0E2D\u0E01\
  \u0E32\u0E23\u0E14\u0E35\u0E1A\u0E31\u0E4A\u0E01\u0E42\u0E14\u0E22\u0E21\u0E35\u0E25\
  \u0E31\u0E01\u0E29\u0E13\u0E30\u0E43\u0E2B\u0E49\u0E02\u0E49\u0E2D\u0E40\u0E2A\u0E19\
  \u0E2D\u0E41\u0E19\u0E30\u0E41\u0E1A\u0E1A\u0E17\u0E31\u0E19\u0E17\u0E35\u0E41\u0E25\
  \u0E30\u0E2A\u0E48\u0E07\u0E40\u0E2A\u0E23\u0E34\u0E21\u0E01\u0E32\u0E23\u0E17\u0E14\
  \u0E2A\u0E2D\u0E1A\u0E41\u0E1A\u0E1A\u0E27\u0E19\u0E25\u0E39\u0E1B."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
weight: 34
---

## อะไรและทำไม?

การใช้งาน interactive shell (REPL - Read-Evaluate-Print Loop) สำหรับ Dart ทำให้โปรแกรมเมอร์สามารถพิมพ์และดำเนินการรันโค้ด Dart ทีละบรรทัดโดยไม่ต้องคอมไพล์สคริปต์ทั้งหมด。เครื่องมือนี้มีค่ามากสำหรับการเรียนรู้ไวยากรณ์ของ Dart, ทดลองกับส่วนต่างๆ ของโค้ดหรือการดีบั๊กโดยมีลักษณะให้ข้อเสนอแนะแบบทันทีและส่งเสริมการทดสอบแบบวนลูป

## วิธีการ:

Dart ไม่มี REPL ภายในตัว อย่างไรก็ตาม, คุณสามารถทำให้มีฟังก์ชั่นคล้ายกับ REPL ได้โดยการใช้ DartPad (ออนไลน์) หรือใช้เครื่องมือของบุคคลที่สามเช่น `dart_repl`.

**การใช้ DartPad:**

DartPad (https://dartpad.dev) คือเว็บไซต์แก้ไขโค้ด Dart ออนไลน์ที่ช่วยให้คุณเขียนและรันโค้ด Dart ในเว็บเบราว์เซอร์ของคุณ เพิ่มเติมไม่ใช่ REPL แบบดั้งเดิมที่ใช้คอมมานด์ไลน์, แต่มันก็ให้ประสบการณ์ที่คล้ายกันสำหรับการทดลองอย่างรวดเร็ว

เพียงไปที่เว็บไซต์, พิมพ์โค้ด Dart ของคุณในแผงซ้าย, แล้วคลิก "Run" เพื่อดูผลลัพธ์ทางด้านขวา

ตัวอย่าง:
```dart
void main() {
  print('Hello, Dart!');
}
```
ผลลัพธ์:
```
Hello, Dart!
```

**การใช้ `dart_repl` (เครื่องมือของบุคคลที่สาม):**

เริ่มแรก, ติดตั้ง `dart_repl` ผ่าน pub แบบโกลบอล:

```shell
dart pub global activate dart_repl
```

จากนั้น, รัน `dart_repl` จากเทอร์มินอลของคุณ:

```shell
dart_repl
```

ตอนนี้, คุณสามารถเริ่มพิมพ์คำสั่ง Dart โดยตรงเข้าไปใน shell ได้เลย เช่น:

```dart
>>> print('Hello, REPL!');
Hello, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

วิธีเหล่านี้ช่วยให้คุณลองโค้ด Dart ได้อย่างรวดเร็ว, ลดความลำบากในการเรียนรู้และเพิ่มประสิทธิภาพได้เป็นอย่างมาก

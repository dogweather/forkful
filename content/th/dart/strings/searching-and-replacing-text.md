---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:50.086445-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Dart \u0E21\u0E35\u0E27\u0E34\
  \u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E41\u0E02\u0E47\u0E07\u0E41\u0E01\
  \u0E23\u0E48\u0E07\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E04\u0E49\
  \u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\u0E42\u0E14\u0E22\
  \u0E43\u0E0A\u0E49\u0E04\u0E25\u0E32\u0E2A `String` \u0E42\u0E14\u0E22\u0E44\u0E21\
  \u0E48\u0E15\u0E49\u0E2D\u0E07\u0E1E\u0E36\u0E48\u0E07\u0E1E\u0E34\u0E07\u0E44\u0E25\
  \u0E1A\u0E23\u0E32\u0E23\u0E35\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01 \u0E19\u0E35\u0E48\
  \u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E2A\
  \u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E44\u0E14\u0E49: #."
lastmod: '2024-03-17T21:57:55.880496-06:00'
model: gpt-4-0125-preview
summary: "Dart \u0E21\u0E35\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\
  \u0E41\u0E02\u0E47\u0E07\u0E41\u0E01\u0E23\u0E48\u0E07\u0E2A\u0E33\u0E2B\u0E23\u0E31\
  \u0E1A\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E42\u0E14\u0E22\
  \u0E15\u0E23\u0E07\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E04\u0E25\u0E32\u0E2A `String`\
  \ \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E1E\u0E36\u0E48\
  \u0E07\u0E1E\u0E34\u0E07\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E20\u0E32\u0E22\
  \u0E19\u0E2D\u0E01 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\
  \u0E35\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E44\
  \u0E14\u0E49."
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 10
---

## วิธีการ:
Dart มีวิธีการที่แข็งแกร่งสำหรับการค้นหาและแทนที่ข้อความโดยตรงโดยใช้คลาส `String` โดยไม่ต้องพึ่งพิงไลบรารีภายนอก นี่คือวิธีที่คุณสามารถทำได้:

### การค้นหาและแทนที่พื้นฐาน
เพื่อค้นหาสตริงย่อยและแทนที่ด้วยสตริงอื่น, คุณสามารถใช้ `replaceAll`:

```dart
String sampleText = "Hello, Dart! Dart is great.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // ผลลัพธ์: Hello, Flutter! Flutter is great.
```

### การใช้ Regular Expressions
สำหรับความต้องการในการค้นหาและแทนที่ที่ซับซ้อนขึ้น, Dart ใช้ regular expressions ผ่านคลาส `RegExp` ซึ่งทำให้สามารถจับคู่แพทเทิร์นและแทนที่ในสตริงได้:

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // ผลลัพธ์: Dart 2024, Flutter 2024
```

ตัวอย่างนี้ค้นหาทุกตัวอย่างของหนึ่งหรือมากกว่าหนึ่งตัวเลข (`\d+`) ในสตริงและแทนที่ด้วย "2024".

### การค้นหาโดยไม่คำนึงถึงตัวเล็กตัวใหญ่
เพื่อทำการค้นหาโดยไม่คำนึงถึงตัวเล็กตัวใหญ่, คุณสามารถแก้ไขคอนสตรัคเตอร์ `RegExp` เพื่อไม่พิจารณาตัวพิมพ์:

```dart
String sampleText = "Welcome to Dart, the programming language.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // ผลลัพธ์: Welcome to Flutter, the programming language.
```

### การแทนที่ด้วยฟังก์ชัน
สำหรับการแทนที่ที่เป็นไดนามิกตามตัวแมทช์เอง, Dart อนุญาตให้ส่งฟังก์ชันไปยัง `replaceAllMapped` ฟังก์ชันนี้สามารถทำการดำเนินการหรือการคำนวณบนลำดับที่ตรงกัน:

```dart
String sampleText = "Increment 5 by 1 to get 6.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // ผลลัพธ์: Increment 6 by 1 to get 7.
```

สิ่งนี้แทนที่ลำดับตัวเลขทุกตัวด้วยค่าการเพิ่มขึ้น ลำดับที่ตรงกันถูกแปลงเป็นจำนวนเต็ม, เพิ่มขึ้น, และจากนั้นแปลงกลับเป็นสตริงเพื่อแทนที่

ความสามารถในการจัดการสตริงของ Dart, โดยเฉพาะเพื่อการค้นหาและแทนที่ข้อความ, ทำให้เป็นเครื่องมือที่มีประสิทธิภาพสำหรับการประมวลผลและเตรียมข้อมูลภายในการประยุกต์ใช้ของคุณ ไม่ว่าจะใช้การแทนที่สตริงอย่างตรงไปตรงมาหรือใช้ประโยชน์จากพลังของ regular expressions, Dart มอบความยืดหยุ่นและประสิทธิภาพที่จำเป็นสำหรับการจัดการข้อความอย่างมีประสิทธิผล

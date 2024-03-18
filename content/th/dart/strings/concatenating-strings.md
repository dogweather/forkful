---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:19.529930-06:00
description: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\
  \u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\
  \u0E1A\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2A\u0E2D\
  \u0E07\u0E2D\u0E31\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E21\u0E32\u0E01\u0E01\u0E27\u0E48\
  \u0E32\u0E40\u0E02\u0E49\u0E32\u0E14\u0E49\u0E27\u0E22\u0E01\u0E31\u0E19 \u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\
  \u0E21\u0E2D\u0E23\u0E4C\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E44\u0E14\u0E49\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\u0E49\u0E19, \u0E2A\
  \u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21,\u2026"
lastmod: '2024-03-17T21:57:55.887444-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\
  \u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\
  \u0E1A\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2A\u0E2D\
  \u0E07\u0E2D\u0E31\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E21\u0E32\u0E01\u0E01\u0E27\u0E48\
  \u0E32\u0E40\u0E02\u0E49\u0E32\u0E14\u0E49\u0E27\u0E22\u0E01\u0E31\u0E19 \u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\
  \u0E21\u0E2D\u0E23\u0E4C\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E44\u0E14\u0E49\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\u0E49\u0E19, \u0E2A\
  \u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21,\u2026"
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การต่อสตริงในการเขียนโปรแกรมเกี่ยวข้องกับการรวมสตริงสองอันหรือมากกว่าเข้าด้วยกัน เพื่อให้โปรแกรมเมอร์สามารถจัดการข้อมูลข้อความได้ง่ายขึ้น, สร้างข้อความ, หรือประกอบส่วนของหน้าต่างผู้ใช้แบบไดนามิก.

## วิธีการ:
Dart มีวิธีการต่าง ๆ ที่ตรงไปตรงมาในการต่อสตริง ด้านล่างนี้เป็นวิธีที่พบบ่อยที่สุด:

### การใช้ตัวดำเนินการ `+`
ตัวดำเนินการ `+` เป็นวิธีที่สัญชาตญาณไม่ที่สุดในการเชื่อมสตริง
```dart
String greeting = 'Hello, ' + 'World!';
print(greeting); // ผลลัพธ์: Hello, World!
```

### การใช้เมธอด `concat()`
แม้ว่า Dart จะไม่มีเมธอด `concat()` เหมือนกับภาษาอื่น ๆ การทำเช่นเดียวกันสามารถทำได้โดยใช้ `+` หรือวิธีต่อไปนี้

### การใช้การแทรกสตริง
การแทรกสตริงช่วยให้สามารถฝังตัวแปรโดยตรงในสตริง ซึ่งมีประสิทธิภาพในการรวมสตริงและนิพจน์
```dart
String user = 'Jane';
String message = 'Welcome, $user!';
print(message); // ผลลัพธ์: Welcome, Jane!
```

### การใช้เมธอด `join()`
เมธอด `join()` เป็นประโยชน์เมื่อคุณมีรายการของสตริงที่คุณต้องการต่อ
```dart
var words = ['Hello', 'from', 'Dart'];
String sentence = words.join(' '); // รวมโดยใช้ตัวคั่นวรรค
print(sentence); // ผลลัพธ์: Hello from Dart
```

### การใช้ StringBuffer
`StringBuffer` เป็นประสิทธิภาพสำหรับการต่อสตริงหลายครั้ง โดยเฉพาะอย่างยิ่งในลูป
```dart
var words = ['Dart', 'is', 'fun'];
StringBuffer buffer = StringBuffer();
for (String word in words) {
  buffer.write(word); // ต่อท้ายแต่ละคำลงในบัฟเฟอร์
  buffer.write(' '); // อาจเพิ่มวรรคตามต้องการ
}
String sentence = buffer.toString().trim(); // แปลงเป็นสตริงและลบวรรคท้าย
print(sentence); // ผลลัพธ์: Dart is fun
```

### ไลบรารีของบุคคลที่สาม
แม้ว่าไลบรารีมาตรฐานของ Dart มักจะเพียงพอสำหรับงานการต่อสตริง ไลบรารีของบุคคลที่สามเช่น `quiver` เสนอยูทิลิตี้ที่สามารถเติมเต็มฟังก์ชันการทำงานของ Dart ได้ ตัวอย่างเช่น, ฟังก์ชัน `concat()` หรือ `merge()` ของ `quiver` อาจถูกสำรวจสำหรับสถานการณ์ขั้นสูง อย่างไรก็ตาม, ควรยึดติดกับตัวเลือกภายในที่แข็งแกร่งของ Dart หากคุณไม่มีความต้องการเฉพาะที่พวกเขาไม่ครอบคลุม.

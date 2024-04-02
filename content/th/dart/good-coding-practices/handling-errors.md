---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:49.770059-06:00
description: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\
  \u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E43\u0E19 Dart\u2026"
lastmod: '2024-03-17T21:57:55.906317-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\
  \u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E43\u0E19 Dart\u2026"
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14"
weight: 16
---

## อะไรและทำไม?
การจัดการข้อผิดพลาดใน Dart เป็นเรื่องของการคาดการณ์และจัดการกับข้อยกเว้นที่เกิดขึ้นระหว่างการทำงานของโปรแกรมเพื่อเพิ่มความน่าเชื่อถือและความสามารถในการใช้งาน โปรแกรมเมอร์ดำเนินการจัดการข้อผิดพลาดเพื่อป้องกันการขัดข้องและให้ข้อเสนอแนะที่มีความหมายกับผู้ใช้ มั่นใจในประสบการณ์แอปพลิเคชันที่ราบรื่นและปลอดภัยยิ่งขึ้น

## วิธีการ:
Dart รองรับข้อผิดพลาดสองประเภท: ข้อผิดพลาดใน*ช่วงเวลาคอมไพล์* และข้อผิดพลาดใน*ช่วงเวลารันไทม์* ข้อผิดพลาดช่วงเวลาคอมไพล์จะถูกตรวจจับโดย Dart analyzer ก่อนที่โค้ดจะทำงาน ในขณะที่ข้อผิดพลาดช่วงเวลารันไทม์ หรือข้อยกเว้น เกิดขึ้นระหว่างการทำงาน นี่คือวิธีที่คุณจัดการกับข้อยกเว้นใน Dart:

### Try-Catch
ใช้ `try-catch` เพื่อจับข้อยกเว้นและป้องกันไม่ให้มันทำให้แอปพลิเคชันของคุณขัดข้อง:

```dart
try {
  var result = 100 ~/ 0; // พยายามทำการหารด้วยศูนย์ แล้วทำให้เกิดข้อยกเว้น
} catch (e) {
  print('Caught an exception: $e'); // จัดการกับข้อยกเว้น
}
```
ตัวอย่างผลลัพธ์: `Caught an exception: IntegerDivisionByZeroException`

### ข้อยกเว้นเฉพาะ
เพื่อจัดการข้อยกเว้นเฉพาะ ให้ระบุข้อยกเว้นหลังจาก `catch`:

```dart
try {
  var result = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('Cannot divide by zero.'); // จัดการเฉพาะการหารด้วยศูนย์
}
```
ตัวอย่างผลลัพธ์: `Cannot divide by zero.`

### ร่องรอยสแต็ค
เพื่อดูร่องรอยสแต็คสำหรับการดีบัก ใช้พารามิเตอร์ที่สองในบล็อก catch:

```dart
try {
  var result = 100 ~/ 0;
} catch (e, s) {
  print('Exception: $e');
  print('Stack trace: $s'); // พิมพ์ร่องรอยสแต็คสำหรับการดีบัก
}
```

### Finally
ใช้ `finally` เพื่อเรียกใช้โค้ดหลังจาก try/catch ไม่ว่าข้อยกเว้นจะถูกโยนหรือไม่:

```dart
try {
  var result = 100 ~/ 0;
} catch (e) {
  print('Caught an exception: $e');
} finally {
  print('This is always executed.'); // โค้ดทำความสะอาดหรือขั้นตอนสุดท้าย
}
```
ตัวอย่างผลลัพธ์:
```
Caught an exception: IntegerDivisionByZeroException
This is always executed.
```

### ไลบรารีของบุคคลที่สาม
แม้ว่าไลบรารีหลักของ Dart จะแข็งแกร่งสำหรับการจัดการข้อผิดพลาด คุณยังสามารถใช้แพ็กเกจของบุคคลที่สามเช่น `dartz` สำหรับการเขียนโปรแกรมแบบฟังก์ชัน ซึ่งนำเสนอแนวคิดเช่น `Either` และ `Option` ที่สามารถใช้สำหรับการจัดการข้อผิดพลาด นี่คือตัวอย่างการใช้ `dartz` สำหรับการจัดการข้อผิดพลาด:

1. เพิ่ม `dartz` ไปยังไฟล์ `pubspec.yaml` ของคุณภายใต้ dependencies:
```yaml
dependencies:
  dartz: ^0.10.0
```

2. ใช้ `Either` สำหรับการจัดการข้อผิดพลาดอย่างประณีประนอมในโค้ด Dart ของคุณ:
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int dividend, int divisor) {
  if (divisor == 0) {
    return Left('Cannot divide by zero.');
  } else {
    return Right(dividend ~/ divisor);
  }
}

void main() {
  final result = divide(100, 0);
  result.fold(
    (left) => print('Error: $left'), 
    (right) => print('Result: $right')
  );
}
```
ตัวอย่างผลลัพธ์: `Error: Cannot divide by zero.`

ส่วน `Left` โดยปกติแสดงถึงข้อผิดพลาด และส่วน `Right` แสดงถึงความสำเร็จ รูปแบบนี้ช่วยให้สามารถจัดการกับข้อผิดพลาดได้อย่างมีฟังก์ชันมากขึ้น มอบความชัดเจนและความควบคุมเหนือการจัดการข้อผิดพลาด

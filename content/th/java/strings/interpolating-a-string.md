---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:49.649281-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Java \u0E44\u0E14\u0E49\u0E40\
  \ introduced `String.format()` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\
  \u0E41\u0E17\u0E23\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07."
lastmod: '2024-03-17T21:57:56.066537-06:00'
model: gpt-4-0125-preview
summary: "Java \u0E44\u0E14\u0E49\u0E40 introduced `String.format()` \u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E2A\u0E15\u0E23\u0E34\
  \u0E07."
title: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E04\u0E48\u0E32\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 8
---

## วิธีการ:
Java ได้เ introduced `String.format()` สำหรับการแทรกสตริง:

```java
public class StringInterpolationExample {
  public static void main(String[] args) {
    String user = "Alice";
    int points = 1337;
    String greeting = String.format("Hi, %s! You have %d points.", user, points);
    System.out.println(greeting);
  }
}
```
ตัวอย่างผลลัพธ์:
```
Hi, Alice! You have 1337 points.
```

สำหรับการแทรกสตริงสมัยใหม่ตั้งแต่ Java 15, เราใช้ text blocks และ `formatted()`:

```java
public class ModernStringInterpolationExample {
  public static void main(String[] args) {
    String user = "Bob";
    double accountBalance = 1234.56;
    String message = """
      Dear %s,
      Your current balance is $%.2f.
      """.formatted(user, accountBalance);
    System.out.println(message);
  }
}
```
ตัวอย่างผลลัพธ์:
```
Dear Bob,
Your current balance is $1234.56.
```

## ลงลึก
ก่อนมีการแทรกสตริง, Java พึ่งพาการต่อสตริง: `String greeting = "Hello, " + user + "!";`. เป็นอะไรที่ลำบากและเสี่ยงต่อข้อผิดพลาด, โดยเฉพาะเมื่อสตริงมีความซับซ้อน

ในอดีต, ภาษาเช่น Perl และ PHP มีการแทรกสตริง Java ตามทันในเวลาหลังมาก `String.format()` และ `PrintStream.printf()` เสนอความสามารถที่คล้ายกัน, โดยใช้ตัวกำหนดรูปแบบที่บอก Java วิธีการจัดการตัวแปร

ทางเลือกอื่น? นอกจาก `String.format()`, เรายังมี `MessageFormat` และ `StringBuilder`, แต่พวกมันไม่สะดวกเหมือนกันสำหรับการแทรกสตริงพื้นฐาน ตั้งแต่ Java 15, text blocks ได้ทำให้สตริงหลายบรรทัดเรียบง่ายขึ้นและเพิ่ม `formatted()` เพื่อง่ายต่อการแทรกสตริงโดยตรงในตำแหน่ง

ในแง่ของการทำงาน, `String.format()` ใช้ `Formatter`, เครื่องมือที่มีความสามารถมากมายในการจัดรูปแบบ แต่ระวัง, สตริงที่ซับซ้อนอาจทำให้ประสิทธิภาพแอปของคุณตกต่ำได้หากคุณไม่ระมัดระวัง

## ดูเพิ่มเติม
- [String (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Formatter (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/util/Formatter.html)
- [JEP 378: Text Blocks (Final)](https://openjdk.java.net/jeps/378)

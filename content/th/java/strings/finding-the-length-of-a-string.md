---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:51.363899-06:00
description: "\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\
  \u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E27\u0E48\u0E32\u0E21\u0E35\
  \u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E01\u0E35\u0E48\u0E15\u0E31\u0E27\u0E43\u0E19\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E19\u0E31\u0E49\u0E19 \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E21\u0E31\u0E01\u0E17\u0E33\u0E01\u0E32\
  \u0E23\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\
  \u0E2D\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E19\u0E33\u0E40\u0E02\u0E49\u0E32\
  , \u0E27\u0E19\u0E0B\u0E49\u0E33\u0E1C\u0E48\u0E32\u0E19\u0E2D\u0E31\u0E01\u0E02\
  \u0E23\u0E30, \u0E2B\u0E23\u0E37\u0E2D\u0E08\u0E31\u0E14\u0E15\u0E33\u0E41\u0E2B\
  \u0E19\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
lastmod: '2024-03-17T21:57:56.071269-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\
  \u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E27\u0E48\u0E32\u0E21\u0E35\
  \u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E01\u0E35\u0E48\u0E15\u0E31\u0E27\u0E43\u0E19\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E19\u0E31\u0E49\u0E19 \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E21\u0E31\u0E01\u0E17\u0E33\u0E01\u0E32\
  \u0E23\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\
  \u0E2D\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E19\u0E33\u0E40\u0E02\u0E49\u0E32\
  , \u0E27\u0E19\u0E0B\u0E49\u0E33\u0E1C\u0E48\u0E32\u0E19\u0E2D\u0E31\u0E01\u0E02\
  \u0E23\u0E30, \u0E2B\u0E23\u0E37\u0E2D\u0E08\u0E31\u0E14\u0E15\u0E33\u0E41\u0E2B\
  \u0E19\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
title: "\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\u0E02\
  \u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 7
---

## อะไร & ทำไม?
การหาความยาวของสตริงหมายถึงการค้นหาว่ามีอักขระกี่ตัวในสตริงนั้น โปรแกรมเมอร์มักทำการนี้เพื่อตรวจสอบข้อมูลนำเข้า, วนซ้ำผ่านอักขระ, หรือจัดตำแหน่งข้อความ

## วิธีการ:
สตริงในภาษา Java มีเมธอด `length()` เพียงเรียกเมธอดนี้ คุณจะได้รับจำนวนอักขระ ง่ายดาย

```java
public class StringLengthExample {
    public static void main(String[] args) {
        String greeting = "Hello, World!";
        int length = greeting.length();

        System.out.println("ความยาวของสตริงคือ: " + length);
        // ผลลัพธ์: ความยาวของสตริงคือ: 13
    }
}
```

## การศึกษาลึกลงไป
เมธอด `length()` มีมาตั้งแต่เวอร์ชั่นแรกๆ ของ Java ทำให้เป็นส่วนหนึ่งที่มีมายาวนานของคลาส `String` มันเรียบง่ายแต่จำเป็น จากภายใน, `String` ใน Java เก็บไว้ด้วยอาร์เรย์ของอักขระ, โดยเมธอด `length()` นั้นจะคืนค่าขนาดของอาร์เรย์นี้ สำคัญที่สุดคือ สตริงใน Java นั้นไม่สามารถเปลี่ยนแปลงได้, ดังนั้นเมื่อสร้างขึ้นแล้ว ความยาวจะไม่เปลี่ยนแปลง, ทำให้เมธอดนี้รวดเร็วและเชื่อถือได้

ทางเลือกอื่น? นอกเหนือจากการสร้างฟังก์ชันของคุณเองเพื่อนับอักขระ (ซึ่งไม่จำเป็นและไม่มีประสิทธิภาพ), ไม่มีจริงๆ นึกถึงว่าเมธอด `length()` คืนค่าจำนวนหน่วย `char`, ไม่ใช่จุดโค้ดโดยตรง สำหรับอักขระ Unicode ที่ไม่เข้ากับขนาด `char` มาตรฐาน 16 บิท, พิจารณาใช้ `codePointCount()` หากคุณต้องการนับอักขระเสริม

## ดูเพิ่มเติม
ศึกษาลึกลงไปหรือสำรวจหัวข้อที่เกี่ยวข้อง:
- [เอกสาร Java String](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [เอกสารคลาส Java Character](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Character.html) เพื่อการเข้าใจมากขึ้นเกี่ยวกับ Unicode, อักขระ, และจุดโค้ด
- [บทเรียนของ Java จาก Oracle](https://docs.oracle.com/javase/tutorial/java/data/strings.html) เพื่อความเข้าใจที่กว้างขึ้นเกี่ยวกับสตริงใน Java

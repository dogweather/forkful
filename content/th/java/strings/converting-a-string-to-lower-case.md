---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:38.069291-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E04\u0E25\u0E32\u0E2A\
  \ `String` \u0E43\u0E19 Java \u0E21\u0E35\u0E40\u0E21\u0E17\u0E2D\u0E14 `toLowerCase()`\
  \ \u0E17\u0E35\u0E48\u0E17\u0E33\u0E07\u0E32\u0E19\u0E2B\u0E19\u0E31\u0E01\u0E43\
  \u0E2B\u0E49\u0E04\u0E38\u0E13\u0E41\u0E25\u0E49\u0E27 \u0E25\u0E2D\u0E07\u0E14\u0E39\
  \u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E07\u0E48\u0E32\u0E22\u0E14\u0E32\u0E22\u0E19\u0E35\u0E49."
lastmod: '2024-03-17T21:57:56.067384-06:00'
model: gpt-4-0125-preview
summary: "\u0E04\u0E25\u0E32\u0E2A `String` \u0E43\u0E19 Java \u0E21\u0E35\u0E40\u0E21\
  \u0E17\u0E2D\u0E14 `toLowerCase()` \u0E17\u0E35\u0E48\u0E17\u0E33\u0E07\u0E32\u0E19\
  \u0E2B\u0E19\u0E31\u0E01\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E41\u0E25\u0E49\u0E27\
  \ \u0E25\u0E2D\u0E07\u0E14\u0E39\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\
  \u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E14\u0E32\u0E22\u0E19\
  \u0E35\u0E49."
title: "\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E47\u0E01"
weight: 4
---

## วิธีการ:
คลาส `String` ใน Java มีเมทอด `toLowerCase()` ที่ทำงานหนักให้คุณแล้ว ลองดูการใช้งานอย่างง่ายดายนี้:

```java
public class LowerCaseExample {
    public static void main(String[] args) {
        String original = "Java ROCKS!";
        String lowerCased = original.toLowerCase();
        System.out.println(lowerCased);
    }
}
```

ผลลัพธ์:

```
java rocks!
```

นั่นคือทั้งหมด สตริงก็จะถูกปรับเสียงให้เบาลงสบายๆ ไปกับตัวพิมพ์เล็ก

## ดำดิ่งลึกลงไป
ครั้งหนึ่งซึ่งการจัดการข้อความนั้นเป็นเรื่องยากลำบาก วัฒนธรรมทางภาษาที่เบี่ยงเบนกัน, การใช้ตัวพิมพ์, ระบบคอมพิวเตอร์ที่กำลังตะโกนในความสับสน Java, ที่มาเข้าสู่วงการในยุค 90, พยายามทำให้สิ่งต่างๆ ง่ายขึ้น เมทอด `toLowerCase()` ได้เป็นส่วนหนึ่งของคลาส `String` ของ Java ตั้งแต่ยุคเริ่มต้น

แต่มีสาระน่าสนใจอยู่ข้างใต้หัวข้อ คุณอาจสงสัยว่าทำไม `toLowerCase()` จึงถูกกำหนดให้มีความจำเป็น สิ่งที่จริงคือไม่ทุกวัฒนธรรมนิยาม "ตัวพิมพ์เล็ก" ในทางเดียวกัน เมทอดนี้ใส่ใจต่อภาษาท้องถิ่น, ใช้ภาษาท้องถิ่นเริ่มต้นของระบบของคุณ, หรือคุณสามารถระบุได้โดยใช้ `toLowerCase(Locale locale)`.

นี่เป็นอีกแง่มุมหนึ่ง: ภาษาที่มีอักษรที่ซับซ้อนมากขึ้น เช่น ตุรกี, มีอักษร "i" ที่ไม่มีจุดที่อาจทำให้การเปลี่ยนเป็นตัวพิมพ์เล็กด้วยวิธีปกติไม่ได้ผล ดังนั้น, Java จึงให้ตัวเลือกในการเปลี่ยนแปลงอักษรอย่างละเอียด

ทางเลือก? แน่นอน, คุณอาจวนรอบสตริงด้วยลูป `for`, แลกเปลี่ยนอักขระด้วยตัวเอง แต่ทำไมต้องประดิษฐ์ล้อใหม่เมื่อ Java ช่วยคุณได้?

นอกจากนี้, สิ่งนี้อาจทำให้บางคนประหลาดใจ: สตริงใน Java เป็น immutable หมายความว่าเมื่อคุณ `toLowerCase()`, คุณไม่ได้เปลี่ยนแปลงสตริงเดิม แต่คุณกำลังสร้างขึ้นใหม่อย่างสมบูรณ์

## ดูเพิ่มเติม
ตรวจสอบแหล่งข้อมูลเหล่านี้เพื่อปรับปรุงเกมสตริงของคุณ:

- Java String API: [](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- Java Locale Class: [](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Locale.html)
- การแมพพิงตัวพิมพ์ใน Unicode: [](https://unicode.org/reports/tr21/)

และสำหรับรายละเอียดเกี่ยวกับมาตรฐาน Unicode:

- สมาคม Unicode: [](https://unicode.org/)

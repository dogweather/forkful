---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:27.639618-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E15\u0E48\u0E2D\u0E44\u0E1B\
  \u0E19\u0E35\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\u0E07\u0E48\u0E32\
  \u0E22\u0E46\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\
  \u0E19\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\
  \u0E36\u0E01\u0E25\u0E47\u0E2D\u0E01\u0E43\u0E19 Java \u0E42\u0E14\u0E22\u0E43\u0E0A\
  \u0E49\u0E41\u0E1E\u0E47\u0E01\u0E40\u0E01\u0E08 `java.util.logging` \u0E17\u0E35\
  \u0E48\u0E21\u0E35\u0E43\u0E2B\u0E49\u0E43\u0E19\u0E15\u0E31\u0E27."
lastmod: '2024-03-17T21:57:56.088099-06:00'
model: gpt-4-0125-preview
summary: "\u0E15\u0E48\u0E2D\u0E44\u0E1B\u0E19\u0E35\u0E49\u0E40\u0E1B\u0E47\u0E19\
  \u0E27\u0E34\u0E18\u0E35\u0E07\u0E48\u0E32\u0E22\u0E46\u0E43\u0E19\u0E01\u0E32\u0E23\
  \u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\
  \u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E25\u0E47\u0E2D\u0E01\u0E43\
  \u0E19 Java \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E41\u0E1E\u0E47\u0E01\u0E40\u0E01\
  \u0E08 `java.util.logging` \u0E17\u0E35\u0E48\u0E21\u0E35\u0E43\u0E2B\u0E49\u0E43\
  \u0E19\u0E15\u0E31\u0E27."
title: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E25\u0E47\u0E2D\u0E01"
weight: 17
---

## วิธีทำ:
ต่อไปนี้เป็นวิธีง่ายๆในการเริ่มต้นใช้งานการบันทึกล็อกใน Java โดยใช้แพ็กเกจ `java.util.logging` ที่มีให้ในตัว

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("บันทึกข้อความระดับ INFO");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "เกิดข้อยกเว้น", e);
        }
    }
}
```

นี่จะทำให้ผลลัพธ์แสดงข้อความดังนี้:

```
Jul 03, 2023 2:00:00 PM AppLogging main
INFO: บันทึกข้อความระดับ INFO
Jul 03, 2023 2:00:00 PM AppLogging main
SEVERE: เกิดข้อยกเว้น
java.lang.ArithmeticException: / by zero
    at AppLogging.main(AppLogging.java:10)
```

## การศึกษาเพิ่มเติม
การ log ใน Java ได้พัฒนาไปอย่างมาก ในอดีต, การบันทึกล็อกมักจะทำอย่างไม่เป็นระบบด้วยการแสดงผลของระบบและกลไกที่เขียนเอง อย่างไรก็ตาม, ความจำเป็นในการมาตฐานของการ log นำไปสู่การเกิดขึ้นของ API สำหรับการ log เช่น `Log4j` และ `SLF4J` แพ็กเกจ `java.util.logging` ถูกนำมาใช้ใน JDK 1.4, ส่งเสริมให้มีการ log ข้อความในแบบมาตรฐาน

ทางเลือกอื่นๆ สำหรับ `java.util.logging` (JUL) ได้แก่ Log4j 2 และ SLF4J ในขณะที่ JUL ถูกรวมอยู่ใน Java และไม่ต้องการความพึ่งพาเพิ่มเติม, ทั้ง Log4j 2 และ SLF4J นำเสนอคุณสมบัติขั้นสูงเพิ่มเติม เช่น การควบคุมการกำหนดค่าการ log ที่ละเอียดยิ่งขึ้น, การบันทึกล็อกแบบไม่ต้องรอซิงโครนัส, และประสิทธิภาพที่ดีขึ้น

ในด้านการปรับใช้, การบันทึกล็อกสามารถดำเนินการโดยแบบซิงโครนัส, ซึ่งแต่ละข้อความบันทึกล็อกจะถูกประมวลผลในเธรดที่สร้างข้อความนั้น, หรือแบบไม่ต้องรอซิงโครนัส, ซึ่งข้อความถูกส่งไปยังเธรดอื่น การบันทึกแบบไม่ต้องรอซิงโครนัสสามารถปรับปรุงประสิทธิภาพ แต่ก็เพิ่มความซับซ้อนเนื่องจากต้องการการจัดการความพร้อมใช้งานร่วมและให้แน่ใจว่าข้อความบันทึกล็อกไม่ถูกสูญเสียเมื่อแอปพลิเคชันล่ม

## ดูเพิ่มเติมที่
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [ภาพรวมการบันทึกล็อกอย่างเป็นทางการของ Oracle](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [บทความสอนเกี่ยวกับ java.util.logging](https://www.vogella.com/tutorials/Logging/article.html)

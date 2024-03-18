---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:12.480861-06:00
description: "\u0E01\u0E32\u0E23\u0E44\u0E14\u0E49\u0E21\u0E32\u0E0B\u0E36\u0E48\u0E07\
  \u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\
  \u0E43\u0E19 Java \u0E40\u0E1B\u0E47\u0E19\u0E01\u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\
  \u0E34\u0E19\u0E01\u0E32\u0E23\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E17\u0E35\
  \u0E48\u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\u0E31\
  \u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E27\u0E31\u0E15\u0E16\u0E38\u0E27\u0E31\
  \u0E19\u0E17\u0E35\u0E48\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E14\u0E33\
  \u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E15\u0E48\u0E32\u0E07\u0E46 \u0E40\u0E0A\
  \u0E48\u0E19 \u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01,\u2026"
lastmod: '2024-03-17T21:57:56.092015-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E44\u0E14\u0E49\u0E21\u0E32\u0E0B\u0E36\u0E48\u0E07\
  \u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\
  \u0E43\u0E19 Java \u0E40\u0E1B\u0E47\u0E19\u0E01\u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\
  \u0E34\u0E19\u0E01\u0E32\u0E23\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E17\u0E35\
  \u0E48\u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\u0E31\
  \u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E27\u0E31\u0E15\u0E16\u0E38\u0E27\u0E31\
  \u0E19\u0E17\u0E35\u0E48\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E14\u0E33\
  \u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E15\u0E48\u0E32\u0E07\u0E46 \u0E40\u0E0A\
  \u0E48\u0E19 \u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01,\u2026"
title: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\
  \u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การได้มาซึ่งวันที่ปัจจุบันใน Java เป็นการดำเนินการพื้นฐานที่ช่วยให้โปรแกรมเมอร์สามารถจัดการกับวัตถุวันที่เพื่อการดำเนินการต่างๆ เช่น การบันทึก, การคำนวณวันที่, และเงื่อนไขที่ขึ้นกับเวลา มันมีความสำคัญในแอปพลิเคชันที่การติดตาม, การกำหนดการ, และการวิเคราะห์ข้อมูลเชิงเวลาเป็นสิ่งสำคัญ

## วิธีการ:
Java นำเสนอวิธีการหลายวิธีในการได้มาซึ่งวันที่ปัจจุบัน โดยใช้ทั้งคลาส `java.util.Date` แบบเก่า และแพคเกจ `java.time` ใหม่ (ซึ่งเปิดตัวใน Java 8) ซึ่งมีความหลากหลายและสัญชาตญาณมากยิ่งขึ้น

### การใช้ `java.time.LocalDate`
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // ตัวอย่างผลลัพธ์: 2023-04-01
    }
}
```
### การใช้ `java.time.LocalDateTime`
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // ตัวอย่างผลลัพธ์: 2023-04-01T12:34:56.789
    }
}
```
### การใช้ `java.util.Date` (รุ่นเก่า)
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // ตัวอย่างผลลัพธ์: Sat Apr 01 12:34:56 BST 2023
    }
}
```
### การใช้ไลบรารีอื่น: Joda-Time
ก่อน Java 8, Joda-Time เป็นมาตรฐานที่ยอมรับกันสำหรับวันที่และเวลาใน Java หากคุณกำลังทำงานกับระบบรุ่นเก่าหรือมีความชอบสำหรับ Joda-Time, นี่คือวิธีที่คุณสามารถใช้มันเพื่อได้มาซึ่งวันที่ปัจจุบัน:
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // ตัวอย่างผลลัพธ์: 2023-04-01
    }
}
```
**หมายเหตุ:** ในขณะที่ `java.util.Date` และ Joda-Time ยังถูกใช้งานอยู่, แพคเกจ `java.time` ถูกแนะนำสำหรับโครงการใหม่เนื่องจากมีความไม่เปลี่ยนแปลงและ API ที่ครบถ้วนสำหรับการจัดการวันที่และเวลา

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:02.279368-06:00
description: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E01\u0E32\
  \u0E23\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E42\u0E22\
  \u0E19\u0E40\u0E01\u0E23\u0E47\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\u0E25\
  \u0E47\u0E01\u0E46 \u0E19\u0E49\u0E2D\u0E22\u0E46 \u0E40\u0E02\u0E49\u0E32\u0E44\
  \u0E1B\u0E43\u0E19\u0E04\u0E2D\u0E19\u0E42\u0E0B\u0E25\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E15\u0E34\u0E14\u0E15\u0E32\u0E21\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14 \u0E21\u0E31\u0E19\u0E40\
  \u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E23\u0E27\u0E14\u0E40\
  \u0E23\u0E47\u0E27 \u0E2A\u0E01\u0E1B\u0E23\u0E01\u2026"
lastmod: '2024-03-17T21:57:56.083840-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E01\u0E32\
  \u0E23\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E42\u0E22\
  \u0E19\u0E40\u0E01\u0E23\u0E47\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\u0E25\
  \u0E47\u0E01\u0E46 \u0E19\u0E49\u0E2D\u0E22\u0E46 \u0E40\u0E02\u0E49\u0E32\u0E44\
  \u0E1B\u0E43\u0E19\u0E04\u0E2D\u0E19\u0E42\u0E0B\u0E25\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E15\u0E34\u0E14\u0E15\u0E32\u0E21\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14 \u0E21\u0E31\u0E19\u0E40\
  \u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E23\u0E27\u0E14\u0E40\
  \u0E23\u0E47\u0E27 \u0E2A\u0E01\u0E1B\u0E23\u0E01\u2026"
title: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E49\u0E14"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การพิมพ์ผลการดีบักคือการโยนเกร็ดข้อมูลเล็กๆ น้อยๆ เข้าไปในคอนโซลเพื่อติดตามการค้นหาข้อผิดพลาด มันเป็นวิธีที่รวดเร็ว สกปรก และมีประสิทธิภาพในการทำความเข้าใจว่ามีอะไรเกิดขึ้นภายในโค้ดของคุณเมื่อมันทำงานอย่างมีอิสระ

## วิธีการ:
มาเริ่มเขียนโค้ดบางอย่างบนหน้าจอกัน:

```java
public class DebugExample {
    public static void main(String[] args) {
        int sum = 0;
        for (int i = 1; i <= 10; i++) {
            sum += i;
            System.out.println("Added " + i + ", sum now: " + sum);
        }
    }
}
```

ตัวอย่างนี้จะรวมตัวเลขจาก 1 ถึง 10 และพิมพ์ความคืบหน้า:

```
Added 1, sum now: 1
Added 2, sum now: 3
...
Added 10, sum now: 55
```

## ลงลึก
ก่อนหน้าที่ IDE จะกลายเป็นอัจฉริยะ, การดีบักแบบใช้ printf คือวิธีที่ไปถึง เดี๋ยวนี้ แม้ว่าจะมีจุดหยุดที่ซับซ้อน บางครั้งการใช้ `System.out.println()` ที่วางบริษัทได้ดี เป็นสิ่งที่คุณต้องการเพื่อให้ดาวเคราะห์เรียงตัว

ตัวเลือกอื่น? กรอบการทำงานการล็อกอย่าง Log4J หรือ SLF4J ให้คุณควบคุมข้อมูลการดีบัก แยกมันออกจากผลลัพธ์ของระบบ และช่วยให้คุณสามารถปรับระดับความละเอียดของข้อมูลได้

ในเรื่องของการนำไปใช้งาน, จำไว้ว่า `System.out` เป็นวัตถุ `PrintStream` ที่เริ่มต้นเป็น stdout สามารถถูกเปลี่ยนเพื่อเปลี่ยนทิศทางผลลัพธ์, ทำให้การทดสอบหรือการบันทึกน้อยกวนยิ่งขึ้น

## ดูเพิ่มเติม
- [บทช่วยสอนของ Oracle เกี่ยวกับ I/O Streams](https://docs.oracle.com/javase/tutorial/essential/io/streams.html)
- [แนวทางปฏิบัติที่ดีที่สุดสำหรับการล็อกใน Java](https://www.baeldung.com/java-logging-intro)
- [เอกสารความช่วยเหลือของ SLF4J](http://www.slf4j.org/docs.html)

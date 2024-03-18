---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:36.351660-06:00
description: "REPL (Read-Eval-Print Loop) \u0E40\u0E1B\u0E47\u0E19 shell \u0E41\u0E1A\
  \u0E1A\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E17\u0E35\u0E48\u0E1B\u0E23\u0E30\u0E21\
  \u0E27\u0E25\u0E1C\u0E25\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E08\u0E32\u0E01\u0E1C\
  \u0E39\u0E49\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E40\u0E1E\u0E35\u0E22\u0E07\u0E04\
  \u0E33\u0E2A\u0E31\u0E48\u0E07\u0E40\u0E14\u0E35\u0E22\u0E27, \u0E17\u0E33\u0E01\
  \u0E32\u0E23\u0E23\u0E31\u0E19\u0E42\u0E04\u0E49\u0E14, \u0E41\u0E25\u0E30\u0E04\
  \u0E37\u0E19\u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.082938-06:00'
model: gpt-4-0125-preview
summary: "REPL (Read-Eval-Print Loop) \u0E40\u0E1B\u0E47\u0E19 shell \u0E41\u0E1A\u0E1A\
  \u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E17\u0E35\u0E48\u0E1B\u0E23\u0E30\u0E21\u0E27\
  \u0E25\u0E1C\u0E25\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E08\u0E32\u0E01\u0E1C\u0E39\
  \u0E49\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E40\u0E1E\u0E35\u0E22\u0E07\u0E04\u0E33\
  \u0E2A\u0E31\u0E48\u0E07\u0E40\u0E14\u0E35\u0E22\u0E27, \u0E17\u0E33\u0E01\u0E32\
  \u0E23\u0E23\u0E31\u0E19\u0E42\u0E04\u0E49\u0E14, \u0E41\u0E25\u0E30\u0E04\u0E37\
  \u0E19\u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
---

{{< edit_this_page >}}

## อะไรและทำไม?
REPL (Read-Eval-Print Loop) เป็น shell แบบโต้ตอบที่ประมวลผลคำสั่งจากผู้ใช้งานเพียงคำสั่งเดียว, ทำการรันโค้ด, และคืนผลลัพธ์ เหมาะสำหรับนักพัฒนาที่ต้องการทดลองรหัสโปรแกรมอย่างเร็ว, ดีบัก, หรือการเรียนรู้ เนื่องจากมันให้ผลตอบรับและการทดลองซ้ำได้ทันที

## วิธีการ:
การเริ่มใช้ REPL ใน Java สามารถทำได้ง่ายๆ ด้วยเครื่องมือ `jshell` ที่ได้รับการแนะนำใน Java 9 นี่คือวิธีการเริ่มต้นใช้งานพื้นฐาน:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
| สร้างเมธอด sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

ออกจากโปรแกรมได้ทุกเวลาด้วย `/exit`.

```Java
jshell> /exit
| ลาก่อน
```

## ลงลึก
ก่อนหน้า `jshell`, โปรแกรมเมอร์ Java ไม่มี REPL อย่างเป็นทางการ, ไม่เหมือนกับนักพัฒนา Python หรือ Ruby พวกเขาใช้ IDEs หรือเขียนโปรแกรมเต็มรูปแบบแม้กระทั่งสำหรับงานเล็กๆน้อยๆ `jshell` ถือเป็นการเปลี่ยนแปลงเกมสำคัญตั้งแต่ Java 9, เติมเต็มช่องว่างนั้น

ทางเลือกอื่น ๆ รวมถึงคอมไพเลอร์ออนไลน์หรือปลั๊กอิน IDE, แต่พวกเขาไม่สามารถเทียบได้กับความทันท่วงทีของ `jshell` ในด้านภายใน, `jshell` ใช้ Java Compiler API ในการประมวลผลของส่วนโค้ด, ซึ่งน่าสนใจมาก มันมากกว่าแค่สนามเด็กเล่น - มันสามารถนำเข้าไลบรารี, ระบุคลาส, และอื่น ๆ อีกมากมาย ทำให้มันเป็นเครื่องมือที่มั่นคงสำหรับการสร้างต้นแบบ

## ดูเพิ่มเติม
- [คู่มือผู้ใช้ JShell](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Java Platform, Standard Edition Tools Reference](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [Java Compiler API](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)

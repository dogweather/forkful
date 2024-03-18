---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:55.621790-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\
  \u0E07 standard error \u0E43\u0E19\u0E20\u0E32\u0E29\u0E32 C \u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E41\u0E2A\u0E14\u0E07\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\
  \u0E14\u0E41\u0E25\u0E30\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E27\u0E34\u0E19\u0E34\
  \u0E08\u0E09\u0E31\u0E22\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E01\u0E23\u0E30\u0E41\u0E2A\
  \u0E17\u0E35\u0E48\u0E41\u0E22\u0E01\u0E08\u0E32\u0E01\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E2B\u0E25\u0E31\u0E01\u0E02\u0E2D\u0E07\u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u2026"
lastmod: '2024-03-17T21:57:56.702274-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\
  \u0E07 standard error \u0E43\u0E19\u0E20\u0E32\u0E29\u0E32 C \u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E41\u0E2A\u0E14\u0E07\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\
  \u0E14\u0E41\u0E25\u0E30\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E27\u0E34\u0E19\u0E34\
  \u0E08\u0E09\u0E31\u0E22\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E01\u0E23\u0E30\u0E41\u0E2A\
  \u0E17\u0E35\u0E48\u0E41\u0E22\u0E01\u0E08\u0E32\u0E01\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E2B\u0E25\u0E31\u0E01\u0E02\u0E2D\u0E07\u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การเขียนไปยัง standard error ในภาษา C หมายถึงการนำข้อความแสดงข้อผิดพลาดและข้อมูลวินิจฉัยไปยังกระแสที่แยกจากผลลัพธ์หลักของโปรแกรม โปรแกรมเมอร์ทำเช่นนี้เพื่อแยกข้อความแสดงข้อผิดพลาดออกจากผลลัพธ์มาตรฐาน เพื่อให้ทั้งสองอ่านและประมวลผลแยกกันได้ง่ายขึ้น โดยเฉพาะเมื่อทำการดีบั๊กหรือบันทึกการปฏิบัติการของโปรแกรม

## วิธีการ:

ในภาษา C สามารถใช้กระแส `stderr` เพื่อเขียนข้อความแสดงข้อผิดพลาด ไม่เหมือนกับการเขียนไปยังผลลัพธ์มาตรฐานด้วย `printf`, การเขียนไปยัง `stderr` สามารถทำได้โดยใช้ `fprintf` หรือ `fputs` นี่คือวิธีที่คุณทำได้:

```c
#include <stdio.h>

int main() {
    fprintf(stderr, "This is an error message.\n");

    fputs("This is another error message.\n", stderr);
    
    return 0;
}
```

ผลลัพธ์ตัวอย่าง (ไปยัง stderr):
```
This is an error message.
This is another error message.
```

สำคัญที่จะต้องทราบว่าในขณะที่ผลลัพธ์อาจดูคล้ายกับ `stdout` ในคอนโซล เมื่อใช้การเปลี่ยนทิศทางในเทอร์มินัล ความแตกต่างจะชัดเจนขึ้น:

```sh
$ ./your_program > output.txt
```

คำสั่งนี้เปลี่ยนทิศทางเฉพาะผลลัพธ์มาตรฐานไปยัง `output.txt`, ในขณะที่ข้อความแสดงข้อผิดพลาดยังปรากฏบนหน้าจอ

## การศึกษาเจาะลึก

ความแตกต่างระหว่าง `stdout` และ `stderr` ในระบบที่ใช้ Unix มีมาตั้งแต่วันแรกของภาษา C และ Unix การแยกนี้ช่วยให้มีการจัดการข้อผิดพลาดและการบันทึกได้อย่างมีประสิทธิภาพมากขึ้น เนื่องจากช่วยให้โปรแกรมเมอร์สามารถเปลี่ยนทิศทางข้อความแสดงข้อผิดพลาดได้โดยไม่ขึ้นกับผลลัพธ์มาตรฐานของโปรแกรม ขณะที่ `stderr` เป็น unbuffered โดยค่าเริ่มต้นเพื่อให้มั่นใจว่าข้อความแสดงข้อผิดพลาดสามารถแสดงผลได้ทันที ซึ่งช่วยในการดีบั๊กการล่มสลายและปัญหาสำคัญอื่น ๆ ในขณะที่ `stdout` โดยปกติจะมีการบัฟเฟอร์ หมายความว่าผลลัพธ์อาจล่าช้าจนกว่าบัฟเฟอร์จะถูกล้าง (เช่น การเสร็จสิ้นโปรแกรมหรือการล้างด้วยตนเอง)

ในแอพพลิเคชันสมัยใหม่ การเขียนไปยัง `stderr` ยังคงมีความเกี่ยวข้องอย่างยิ่ง โดยเฉพาะสำหรับเครื่องมือบรรทัดคำสั่งและแอพพลิเคชันเซิร์ฟเวอร์ที่ความแตกต่างระหว่างข้อความบันทึกปกติและข้อผิดพลาดเป็นสิ่งสำคัญ อย่างไรก็ตาม สำหรับการจัดการข้อผิดพลาดที่ซับซ้อนยิ่งขึ้น โดยเฉพาะในแอพพลิเคชันที่มี GUI หรือที่ต้องการกลไกการบันทึกที่ซับซ้อนยิ่งขึ้น โปรแกรมเมอร์อาจใช้ไลบรารีการบันทึกที่เฉพาะเจาะจงซึ่งให้การควบคุมที่ดีกว่าเกี่ยวกับการจัดรูปแบบข้อความ ปลายทาง (เช่น ไฟล์ ตามเครือข่าย) และระดับความรุนแรง (ข้อมูล คำเตือน ข้อผิดพลาด ฯลฯ)

ในขณะที่ `stderr` ให้กลไกพื้นฐานสำหรับการรายงานข้อผิดพลาดในภาษา C การพัฒนาของปฏิบัติการโปรแกรมและการมีอยู่ของเฟรมเวิร์กการบันทึกขั้นสูงหมายความว่ามันมักเป็นเพียงจุดเริ่มต้นสำหรับกลยุทธ์การจัดการข้อผิดพลาดสมัยใหม่
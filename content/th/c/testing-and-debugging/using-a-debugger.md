---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:57.679400-06:00
description: "Debugger \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E20\u0E32\u0E29\u0E32\
  \ C \u0E40\u0E1B\u0E47\u0E19\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\u0E37\
  \u0E2D\u0E1E\u0E34\u0E40\u0E28\u0E29\u0E17\u0E35\u0E48\u0E0A\u0E48\u0E27\u0E22\u0E43\
  \u0E2B\u0E49\u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E40\u0E14\u0E34\u0E19\u0E1C\u0E48\u0E32\u0E19\u0E42\u0E04\u0E49\u0E14\
  \ \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\
  \ \u0E41\u0E25\u0E30\u0E15\u0E34\u0E14\u0E15\u0E32\u0E21\u0E01\u0E32\u0E23\u0E44\
  \u0E2B\u0E25\u0E02\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E44\
  \u0E14\u0E49\u2026"
lastmod: '2024-03-17T21:57:56.691056-06:00'
model: gpt-4-0125-preview
summary: "Debugger \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E20\u0E32\u0E29\u0E32 C\
  \ \u0E40\u0E1B\u0E47\u0E19\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\u0E37\
  \u0E2D\u0E1E\u0E34\u0E40\u0E28\u0E29\u0E17\u0E35\u0E48\u0E0A\u0E48\u0E27\u0E22\u0E43\
  \u0E2B\u0E49\u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E40\u0E14\u0E34\u0E19\u0E1C\u0E48\u0E32\u0E19\u0E42\u0E04\u0E49\u0E14\
  \ \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\
  \ \u0E41\u0E25\u0E30\u0E15\u0E34\u0E14\u0E15\u0E32\u0E21\u0E01\u0E32\u0E23\u0E44\
  \u0E2B\u0E25\u0E02\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E44\
  \u0E14\u0E49\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

Debugger สำหรับภาษา C เป็นเครื่องมือพิเศษที่ช่วยให้นักพัฒนาสามารถเดินผ่านโค้ด ตรวจสอบตัวแปร และติดตามการไหลของการทำงานได้ กระบวนการนี้เป็นส่วนสำคัญในการระบุและแก้ไขข้อผิดพลาด ทำให้มั่นใจว่าโค้ดทำงานได้ตามที่คาดหวัง

## วิธีการ:

GDB (GNU Debugger) เป็น debugger ที่ใช้กันมากที่สุดสำหรับการเขียนโปรแกรมภาษา C นี่คือคู่มือสั้น ๆ เกี่ยวกับการใช้ GDB ในการดีบักโปรแกรม C ง่าย ๆ

ก่อนอื่น คอมไพล์โปรแกรม C ของคุณด้วย flag `-g` เพื่อรวมข้อมูลการดีบัก:

```c
gcc -g program.c -o program
```

จากนั้น เริ่ม GDB พร้อมกับโปรแกรมที่คอมไพล์แล้ว:

```bash
gdb ./program
```

คุณสามารถใช้คำสั่งต่าง ๆ ใน GDB เพื่อควบคุมการทำงานของมัน ต่อไปนี้เป็นคำสั่งพื้นฐานบางส่วน:

- `break`: ตั้งจุดหยุดที่บรรทัดหรือฟังก์ชันที่ระบุเพื่อหยุดการทำงาน
  - ตัวอย่าง: `break 10` หรือ `break main`
- `run`: เริ่มการทำงานของโปรแกรมภายใน GDB
- `next`: ทำงานบรรทัดถัดไปโดยไม่เข้าไปในฟังก์ชัน
- `step`: ทำงานบรรทัดต่อไปโดยเข้าไปในฟังก์ชัน
- `print`: แสดงค่าของตัวแปร
- `continue`: ดำเนินการต่อจนถึงจุดหยุดถัดไป
- `quit`: ออกจาก GDB

นี่คือตัวอย่างของการดีบักโปรแกรมง่าย ๆ:

```c
#include <stdio.h>

int main() {
    int i;
    for (i = 0; i < 5; i++) {
        printf("%d\n", i);
    }
    return 0;
}
```

คอมไพล์และเริ่ม GDB ตามที่อธิบายไว้ ตั้งจุดหยุดที่บรรทัดของ `printf` ด้วย `break 5` แล้ว `run` ใช้ `next` เพื่อเดินผ่านลูป และ `print i` เพื่อตรวจสอบตัวแปรลูป

ผลลัพธ์ตัวอย่างหลังจากตั้งจุดหยุดและก่อนการทำซ้ำครั้งแรก:

```
Breakpoint 1, main () at program.c:5
5         printf("%d\n", i);
```

ใช้ `print i` หลังจากทำซ้ำหลายครั้ง:

```
$3 = 2
```

นี่แสดงการตรวจสอบสภาพและการไหลของโปรแกรมง่าย ๆ

## ข้อลึก

แนวคิดของการดีบักได้พัฒนาขึ้นอย่างมากตั้งแต่ยุคแรก ๆ ของการเขียนโปรแกรม ที่แมลงจริง ๆ (แมลงตัวจริง) อาจทำให้เกิดปัญหาในคอมพิวเตอร์เชิงกล ในปัจจุบัน debugger เช่น GDB มีฟีเจอร์ที่ซับซ้อนกว่าการก้าวเดินและตรวจสอบตัวแปรเบื้องต้น เช่น การดีบักย้อนกลับ (ทำงานโปรแกรมย้อนหลัง) จุดหยุดเงื่อนไข และการเขียนสคริปต์สำหรับงานการดีบักอัตโนมัติ

ในขณะที่ GDB เป็นเครื่องมือที่ทรงพลังและใช้กันอย่างแพร่หลาย อาจยากและท้าทายสำหรับผู้เริ่มต้น ตัวเลือกเครื่องมือการดีบักและ IDE (สภาพแวดล้อมการพัฒนาแบบบูรณาการ) เช่น Visual Studio Code, CLion, หรือ Eclipse นำเสนออินเทอร์เฟซที่เป็นมิตรกับผู้ใช้มากขึ้นสำหรับการดีบักโค้ดภาษา C โดยมักจะรวมเอฟเฟกต์ภาพและการควบคุมที่ชัดเจนกว่า ตัวเลือกเหล่านี้อาจไม่มีความลึกของฟังก์ชันเต็มรูปแบบของ GDB แต่สามารถเข้าถึงได้ง่ายกว่าสำหรับผู้ที่เพิ่งเริ่มต้นเขียนโปรแกรมภาษา C

นอกจากนี้ การปรากฏตัวของโปรโตคอลเซิร์ฟเวอร์ภาษาและมาตรฐานการดีบักได้ช่วยให้มีโซลูชันการดีบักแบบข้ามแพลตฟอร์ม เพิ่มประสบการณ์การดีบักให้สอดคล้องกันมากขึ้นทั่วทุกเครื่องมือและสภาพแวดล้อม แม้ว่าจะมีความก้าวหน้าเหล่านี้ การเรียนรู้ถึงความลึกของ debugger แบบดั้งเดิมเช่น GDB ให้ข้อมูลเชิงลึกอันมีค่าเกี่ยวกับการทำงานของโปรแกรมภาษา C และยังคงเป็นทักษะสำคัญในเครื่องมือของผู้พัฒนา
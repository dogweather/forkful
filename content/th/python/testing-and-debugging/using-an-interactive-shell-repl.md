---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:46.792865-06:00
description: "REPL \u0E2B\u0E23\u0E37\u0E2D Read-Eval-Print Loop \u0E40\u0E1B\u0E47\
  \u0E19\u0E2A\u0E20\u0E32\u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\u0E01\u0E32\
  \u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E17\
  \u0E35\u0E48\u0E23\u0E31\u0E1A\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E08\u0E32\u0E01\
  \u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E40\u0E1E\u0E35\u0E22\u0E07\u0E23\u0E32\u0E22\
  \u0E01\u0E32\u0E23\u0E40\u0E14\u0E35\u0E22\u0E27, \u0E1B\u0E23\u0E30\u0E21\u0E27\
  \u0E25\u0E1C\u0E25, \u0E41\u0E25\u0E30\u0E2A\u0E48\u0E07\u0E1C\u0E25\u0E25\u0E31\
  \u0E1E\u0E18\u0E4C\u0E01\u0E25\u0E31\u0E1A\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E1C\u0E39\
  \u0E49\u0E43\u0E0A\u0E49\u2026"
lastmod: '2024-03-17T21:57:55.764133-06:00'
model: gpt-4-0125-preview
summary: "REPL \u0E2B\u0E23\u0E37\u0E2D Read-Eval-Print Loop \u0E40\u0E1B\u0E47\u0E19\
  \u0E2A\u0E20\u0E32\u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\
  \u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E17\u0E35\
  \u0E48\u0E23\u0E31\u0E1A\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E08\u0E32\u0E01\u0E1C\
  \u0E39\u0E49\u0E43\u0E0A\u0E49\u0E40\u0E1E\u0E35\u0E22\u0E07\u0E23\u0E32\u0E22\u0E01\
  \u0E32\u0E23\u0E40\u0E14\u0E35\u0E22\u0E27, \u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\
  \u0E1C\u0E25, \u0E41\u0E25\u0E30\u0E2A\u0E48\u0E07\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E25\u0E31\u0E1A\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E1C\u0E39\u0E49\
  \u0E43\u0E0A\u0E49\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
---

{{< edit_this_page >}}

## อะไรและทำไม?
REPL หรือ Read-Eval-Print Loop เป็นสภาพแวดล้อมการเขียนโปรแกรมที่รับคำสั่งจากผู้ใช้เพียงรายการเดียว, ประมวลผล, และส่งผลลัพธ์กลับไปยังผู้ใช้ โปรแกรมเมอร์ใช้สำหรับการทดสอบอย่างรวดเร็ว, การเรียนรู้, การดีบัก, หรือการคำนวณตามต้องการ

## วิธีการ:
เริ่มต้นใช้งาน REPL ของ Python ได้โดยพิมพ์ `python` ใน command line ของคุณ เมื่ออยู่ที่นั่น, ทดสอบการทำงานที่ง่ายหรือโค้ดที่มีหลายบรรทัด:

```Python
>>> 1 + 1
2
>>> for i in range(3):
...     print(i)
... 
0
1
2
```

ทดลองใช้ฟังก์ชันและรับข้อเสนอแนะทันที:

```Python
>>> def greet(name):
...     return "Hello, " + name + "!"
... 
>>> greet("Alice")
'Hello, Alice!'
```

เล่นกับไลบรารีและสำรวจคุณสมบัติของพวกเขาแบบเรียลไทม์:

```Python
>>> import math
>>> math.sqrt(16)
4.0
```

ออกด้วยการพิมพ์ `exit()` หรือ `Ctrl+D` (บางครั้งอาจเป็น `Ctrl+Z` บน Windows)

## การศึกษาลึก
คอนเซ็ปต์ของ REPL ไม่เฉพาะเจาะจงกับ Python; มันเก่าแก่เท่ากับ Lisp หลายภาษาเสนอสภาพแวดล้อมที่ตอบสนองแบบทันทีและสามารถโต้ตอบได้สำหรับการใช้วิธีการเข้ามือทำโค้ด ทางเลือกที่แตกต่างจาก shell ของ Python มาตรฐาน รวมถึง IPython และ Jupyter Notebook, ซึ่งให้ความสามารถในการโต้ตอบได้ดียิ่งขึ้น, มีคุณสมบัติเพิ่มเติม, และการรวมกับเครื่องมืออื่นได้ดีกว่า REPL มาตรฐานของ Python นั้นง่าย, แต่มันฝังอำนาจเต็มรูปแบบของ Python, รองรับวัตถุที่ซับซ้อนและโปรแกรมที่ใช้หลายเธรด, อย่างไรก็ตามมันขาดคุณสมบัติเช่น auto-completion และ syntax highlighting ที่มีในเครื่องมือที่ซับซ้อนกว่า

## ดูเพิ่ม
- [เอกสารทางการของ Python เกี่ยวกับตัวแปล](https://docs.python.org/3/tutorial/interpreter.html)
- [IPython: หอสมุด Python ขั้นสูง](https://ipython.org/)
- [โครงการ Jupyter](https://jupyter.org/)

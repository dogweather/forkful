---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:01.615808-06:00
description: "Read-Eval-Print-Loop (REPL) \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E20\u0E32\
  \u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\
  \u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E17\u0E35\u0E48\u0E23\u0E31\
  \u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E19\u0E33\u0E40\u0E02\u0E49\u0E32\u0E02\
  \u0E2D\u0E07\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E40\u0E1E\u0E35\u0E22\u0E07\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E40\u0E14\u0E35\u0E22\u0E27, \u0E1B\u0E23\u0E30\u0E21\
  \u0E27\u0E25\u0E1C\u0E25, \u0E41\u0E25\u0E30\u0E2A\u0E48\u0E07\u0E01\u0E25\u0E31\
  \u0E1A\u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E1C\
  \u0E39\u0E49\u0E43\u0E0A\u0E49\u2026"
lastmod: '2024-03-17T21:57:55.946373-06:00'
model: gpt-4-0125-preview
summary: "Read-Eval-Print-Loop (REPL) \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E20\u0E32\u0E1E\
  \u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\
  \u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E17\u0E35\u0E48\u0E23\u0E31\u0E1A\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E19\u0E33\u0E40\u0E02\u0E49\u0E32\u0E02\u0E2D\
  \u0E07\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E40\u0E1E\u0E35\u0E22\u0E07\u0E2D\u0E22\
  \u0E48\u0E32\u0E07\u0E40\u0E14\u0E35\u0E22\u0E27, \u0E1B\u0E23\u0E30\u0E21\u0E27\
  \u0E25\u0E1C\u0E25, \u0E41\u0E25\u0E30\u0E2A\u0E48\u0E07\u0E01\u0E25\u0E31\u0E1A\
  \u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E1C\u0E39\
  \u0E49\u0E43\u0E0A\u0E49 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E43\u0E0A\u0E49 REPL \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E14\u0E25\
  \u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E0A\u0E34\u0E49\u0E19\u0E2A\u0E48\u0E27\u0E19\u0E02\
  \u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\
  \u0E40\u0E23\u0E47\u0E27, \u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14, \u0E41\u0E25\u0E30\u0E01\
  \u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E19\u0E23\u0E39\u0E49\u0E04\u0E38\u0E13\u0E2A\
  \u0E21\u0E1A\u0E31\u0E15\u0E34\u0E43\u0E2B\u0E21\u0E48\u0E46 \u0E02\u0E2D\u0E07\u0E20\
  \u0E32\u0E29\u0E32\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E2A\
  \u0E23\u0E49\u0E32\u0E07\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\
  \u0E19\u0E41\u0E1A\u0E1A\u0E40\u0E15\u0E47\u0E21\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\
  ."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
weight: 34
---

## อะไร & ทำไม?
Read-Eval-Print-Loop (REPL) เป็นสภาพแวดล้อมการเขียนโปรแกรมที่รับข้อมูลนำเข้าของผู้ใช้เพียงอย่างเดียว, ประมวลผล, และส่งกลับผลลัพธ์ไปยังผู้ใช้ โปรแกรมเมอร์ใช้ REPL เพื่อทดลองกับชิ้นส่วนของโค้ดอย่างรวดเร็ว, การแก้ไขข้อผิดพลาด, และการเรียนรู้คุณสมบัติใหม่ๆ ของภาษาโดยไม่ต้องสร้างแอปพลิเคชันแบบเต็มรูปแบบ

## วิธีการ:
TypeScript ไม่มี REPL ของตัวเอง ลองใช้ `ts-node` สภาพแวดล้อมการประมวลผลของ TypeScript สำหรับ Node.js ซึ่งรวมถึง REPL

ก่อนอื่น, ติดตั้งให้เป็นสากล:
```bash
npm install -g ts-node
```

เริ่ม REPL ด้วยการพิมพ์ `ts-node` ในบรรทัดคำสั่งของคุณ:
```bash
ts-node
```

นี่คือสนิปเพตเพื่อลอง:
```TypeScript
> let message: string = 'Hello, REPL!';
> console.log(message);
Hello, REPL!
> 
```
ให้จบเซสชันโดยกด `Ctrl+D`

## ศึกษาลึก
ในอดีต, REPL มีบทบาทสำคัญในภาษาเช่น Lisp ที่อนุญาตให้ทำการประเมินโค้ดแบบไดนามิก แนวคิดนี้ได้แพร่กระจายออกไป, กลายเป็นสิ่งสำคัญสำหรับการเขียนโค้ดแบบโต้ตอบในหลายภาษา

สำหรับ TypeScript, `ts-node` ไม่ใช่ตัวเลือกเดียวของคุณ ทางเลือกอื่น ๆ รวมถึงการใช้ TypeScript Playground ในเว็บเบราว์เซอร์หรือการใช้ REPL อื่น ๆ ที่ใช้ Node.js ซึ่งรองรับ TypeScript พร้อมปลั๊กอินที่เหมาะสม

ในแง่ของการดำเนินการ, `ts-node` ใช้ API คอมไพเลอร์ของ TypeScript เพื่อแปลโค้ดสดก่อนที่จะถูก Node.js ประมวลผล สิ่งนี้ให้ข้อเสนอแนะทันทีและเป็นประโยชน์โดยเฉพาะในการทดสอบคุณสมบัติล่าสุดของ TypeScript โดยไม่มีความยุ่งยากในการตั้งค่า

สิ่งหนึ่งที่ควรจำ – ในขณะที่ REPL เหมาะสำหรับการทดสอบอย่างรวดเร็ว, มันไม่สามารถแทนที่การเขียนโค้ดแบบดั้งเดิมที่สามารถทดสอบและบำรุงรักษาได้ เป็นเครื่องมือสำหรับการเรียนรู้และการสำรวจ, ไม่ใช่ทางเลือกสำหรับแนวปฏิบัติการพัฒนาที่เหมาะสม

## ดูเพิ่มเติม
- [เว็บไซต์อย่างเป็นทางการของ TypeScript](https://www.typescriptlang.org/)
- [ts-node บน GitHub](https://github.com/TypeStrong/ts-node)
- [เอกสารของ Node.js REPL](https://nodejs.org/api/repl.html)
- [TypeScript Playground](https://www.typescriptlang.org/play)

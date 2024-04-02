---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:15.550100-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E17\u0E35\
  \u0E48\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\
  \u0E1E\u0E25\u0E32\u0E14 (stderr) \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\
  \u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\
  \u0E19\u0E17\u0E34\u0E28\u0E17\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\
  \u0E41\u0E2A\u0E14\u0E07\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\
  \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E27\u0E34\u0E19\u0E34\u0E08\u0E09\u0E31\u0E22\
  \u0E17\u0E35\u0E48\u0E41\u0E22\u0E01\u0E08\u0E32\u0E01\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E2B\u0E25\u0E31\u0E01 \u0E0B\
  \u0E36\u0E48\u0E07\u0E2A\u0E48\u0E07\u0E44\u0E1B\u0E17\u0E35\u0E48\u0E21\u0E32\u0E15\
  \u0E23\u0E10\u0E32\u0E19\u0E2D\u0E2D\u0E01\u2026"
lastmod: '2024-03-17T21:57:56.146432-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E17\u0E35\
  \u0E48\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\
  \u0E1E\u0E25\u0E32\u0E14 (stderr) \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\
  \u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\
  \u0E19\u0E17\u0E34\u0E28\u0E17\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\
  \u0E41\u0E2A\u0E14\u0E07\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\
  \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E27\u0E34\u0E19\u0E34\u0E08\u0E09\u0E31\u0E22\
  \u0E17\u0E35\u0E48\u0E41\u0E22\u0E01\u0E08\u0E32\u0E01\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E2B\u0E25\u0E31\u0E01 \u0E0B\
  \u0E36\u0E48\u0E07\u0E2A\u0E48\u0E07\u0E44\u0E1B\u0E17\u0E35\u0E48\u0E21\u0E32\u0E15\
  \u0E23\u0E10\u0E32\u0E19\u0E2D\u0E2D\u0E01\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19"
weight: 25
---

## อะไรและทำไม?

การเขียนไปที่มาตรฐานข้อผิดพลาด (stderr) เกี่ยวข้องกับการเปลี่ยนทิศทางข้อความแสดงข้อผิดพลาดและการวินิจฉัยที่แยกจากผลลัพธ์โปรแกรมหลัก ซึ่งส่งไปที่มาตรฐานออก (stdout) โปรแกรมเมอร์ทำแบบนี้เพื่อทำให้การจัดการข้อผิดพลาดและการบันทึกข้อมูลง่ายขึ้น โดยเฉพาะอย่างยิ่งในสภาพแวดล้อมที่การแยกแยะผลลัพธ์เป็นสิ่งสำคัญสำหรับการแก้ไขและการตรวจสอบ

## วิธีการ:

Elm มุ่งเน้นไปที่การพัฒนาเว็บ ที่ซึ่งแนวคิดในการเขียนข้อมูลโดยตรงไปยัง stderr ไม่ถูกใช้งานในลักษณะเดียวกับในสภาพแวดล้อม command-line แบบดั้งเดิม อย่างไรก็ตามสำหรับโปรแกรม Elm ที่ทำงานใน Node.js หรือสภาพแวดล้อมที่คล้ายคลึงกัน การใช้พอร์ตเพื่อสื่อสารกับ JavaScript เป็นวิธีการหลักในการบรรลุฟังก์ชั่นที่คล้ายคลึงกัน นี่คือวิธีที่คุณอาจตั้งค่ามันขึ้นมา:

Elm Code (`Main.elm`):
```elm
port module Main exposing (main)

import Browser

port errorOut : String -> Cmd msg

-- ตัวอย่างฟังก์ชั่นที่ส่งข้อความแสดงข้อผิดพลาดไปยัง JS
generateError : String -> Cmd msg
generateError message =
    errorOut message

main =
    generateError "This is an error message for stderr"
```

การสื่อสารกับ JavaScript (`index.js`):
```javascript
const { Elm } = require('./Main.elm');

var app = Elm.Main.init();

app.ports.errorOut.subscribe((message) => {
  console.error(message);
});
```

โค้ด Elm นี้กำหนดพอร์ต `errorOut` ที่อนุญาตให้ส่งข้อความออกจาก Elm ไปยัง JavaScript จากนั้นในโค้ด JavaScript เราฟังข้อความที่ถูกส่งผ่านพอร์ตนี้และเปลี่ยนทิศทางไปยัง stderr โดยใช้ `console.error()` ด้วยวิธีนี้ คุณสามารถเขียนไปยัง stderr ได้อย่างมีประสิทธิภาพในสภาพแวดล้อมที่รองรับ โดยใช้คุณสมบัติการสื่อสารระหว่าง Elm กับ JavaScript

ตัวอย่างผลลัพธ์ใน terminal Node.js (เมื่อรัน `index.js`):
```
This is an error message for stderr
```

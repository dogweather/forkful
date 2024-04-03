---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:31.939633-06:00
description: "\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\
  \u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E44\u0E1B\
  \u0E22\u0E31\u0E07\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E37\u0E48\
  \u0E2D\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E23\u0E31\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E19\u0E31\u0E49\u0E19 \u0E46\u2026"
lastmod: '2024-03-17T21:57:55.960876-06:00'
model: gpt-4-0125-preview
summary: "\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\
  \u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E44\u0E1B\
  \u0E22\u0E31\u0E07\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E37\u0E48\
  \u0E2D\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E23\u0E31\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E19\u0E31\u0E49\u0E19 \u0E46 \u0E19\u0E31\u0E01\u0E1E\u0E31\
  \u0E12\u0E19\u0E32\u0E43\u0E0A\u0E49\u0E1E\u0E27\u0E01\u0E21\u0E31\u0E19\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E1B\u0E23\u0E31\u0E1A\u0E41\u0E15\u0E48\u0E07\u0E1E\u0E24\u0E15\
  \u0E34\u0E01\u0E23\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E40\u0E1B\u0E25\
  \u0E35\u0E48\u0E22\u0E19\u0E42\u0E04\u0E49\u0E14."
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
weight: 23
---

## อะไร & ทำไม?
อาร์กิวเมนต์บรรทัดคำสั่งช่วยให้ผู้ใช้สามารถส่งข้อมูลไปยังโปรแกรมเมื่อพวกเขารันโปรแกรมนั้น ๆ นักพัฒนาใช้พวกมันเพื่อปรับแต่งพฤติกรรมของโปรแกรมโดยไม่ต้องเปลี่ยนโค้ด

## วิธีการ:

ใน TypeScript, คุณใช้ Node.js เพื่ออ่านอาร์กิวเมนต์บรรทัดคำสั่ง นี่คือวิธีการ:

```typescript
// ต้องการไฟล์นี้เพื่อนำเข้า process จาก Node.js
import process from 'process';

// เก็บอาร์กิวเมนต์บรรทัดคำสั่งตั้งแต่ตำแหน่งที่ 3 เป็นต้นไป
const args = process.argv.slice(2);

console.log('อาร์กิวเมนต์บรรทัดคำสั่ง:', args);
```

รันสคริปต์นี้เช่น `ts-node yourscript.ts arg1 arg2` และดู:

```
อาร์กิวเมนต์บรรทัดคำสั่ง: ['arg1', 'arg2']
```

## ลงลึก

ย้อนกลับไปในวันแรก ๆ ของบรรทัดคำสั่ง, การโต้ตอบของผู้ใช้เกี่ยวข้องกับข้อความทั้งหมด Linux, UNIX, และ Windows ใช้อาร์กิวเมนต์บรรทัดคำสั่งเพื่อบอกโปรแกรมว่าต้องทำอะไร

ตอนนี้สำหรับทางเลือกอื่น: นอกจาก `process.argv`, ใน Node.js, คุณสามารถใช้ไลบรารีเช่น `yargs` หรือ `commander` เพื่อคุณสมบัติเพิ่มเติมเช่นการแยกวิเคราะห์และการตรวจสอบความถูกต้อง

หลักการของสิ่งนี้ใน TypeScript นั้นง่ายดาย: `process.argv` เป็นอาร์เรย์ที่มีอาร์กิวเมนต์ทั้งหมด ดัชนี 0 คือพาทไปยัง Node, ดัชนีที่ 1 คือพาทของสคริปต์, ดังนั้นอาร์กิวเมนต์จริงๆ จึงเริ่มตั้งแต่ดัชนีที่ 2

## ดูเพิ่มเติม

เพื่อสำรวจเพิ่มเติม, เริ่มต้นด้วยเหล่านี้:

- [เอกสารของ Node.js process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [ห้องสมุด GitHub ของ Yargs](https://github.com/yargs/yargs)
- [ห้องสมุด GitHub ของ Commander.js](https://github.com/tj/commander.js)

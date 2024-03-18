---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:10.912924-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\
  \u0E33\u0E2A\u0E31\u0E48\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E04\u0E27\u0E49\u0E32\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\u0E2A\u0E23\
  \u0E34\u0E21\u0E17\u0E35\u0E48\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E43\u0E2A\u0E48\
  \u0E44\u0E27\u0E49\u0E01\u0E31\u0E1A\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E40\u0E21\
  \u0E37\u0E48\u0E2D\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E40\u0E23\u0E34\u0E48\u0E21\
  \u0E17\u0E33\u0E07\u0E32\u0E19\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E02\u0E2D\
  \u0E07\u0E04\u0E38\u0E13\u2026"
lastmod: '2024-03-17T21:57:56.617628-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\
  \u0E33\u0E2A\u0E31\u0E48\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E04\u0E27\u0E49\u0E32\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\u0E2A\u0E23\
  \u0E34\u0E21\u0E17\u0E35\u0E48\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E43\u0E2A\u0E48\
  \u0E44\u0E27\u0E49\u0E01\u0E31\u0E1A\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E40\u0E21\
  \u0E37\u0E48\u0E2D\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E40\u0E23\u0E34\u0E48\u0E21\
  \u0E17\u0E33\u0E07\u0E32\u0E19\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E02\u0E2D\
  \u0E07\u0E04\u0E38\u0E13\u2026"
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การอ่านอาร์กิวเมนต์บรรทัดคำสั่งหมายถึงการคว้าข้อมูลเสริมที่ผู้ใช้ใส่ไว้กับคำสั่งเมื่อพวกเขาเริ่มทำงานสคริปต์ของคุณ โปรแกรมเมอร์ทำอย่างนี้เพื่อให้ผู้ใช้สามารถปรับแต่งพฤติกรรมโดยไม่ต้องเปลี่ยนโค้ด

## วิธีการ:
นี่คือวิธีที่ตรงไปตรงมาในการทำบน Node.js:

```javascript
// process.argv ประกอบด้วยอาร์กิวเมนต์บรรทัดคำสั่ง
const args = process.argv.slice(2);

console.log(args);

// รันสคริปต์นี้ด้วย: node yourscript.js firstArg secondArg
```

ตัวอย่างผลลัพธ์เมื่อคุณรัน `node yourscript.js pineapple 42`:

```javascript
['pineapple', '42']
```

การใช้แพ็คเกจเช่น `yargs` ทำให้ชีวิตง่ายขึ้น โดยให้คุณกำหนดและเข้าถึงอาร์กิวเมนต์ด้วยชื่อ

```javascript
// ติดตั้งด้วย npm install yargs
const yargs = require('yargs/yargs');
const { hideBin } = require('yargs/helpers');
const argv = yargs(hideBin(process.argv)).argv;

console.log(argv);

// รันอย่างนี้ด้วย: node yourscript.js --fruit pineapple --number 42
```

และคุณจะได้:

```javascript
{ fruit: 'pineapple', number: '42' }
```

สะอาดและชัดเจน พร้อมพารามิเตอร์ที่มีชื่อ

## ดำดิ่งลึก
ก่อนหน้านี้ อาร์กิวเมนต์ถูกอ่านในภาษา C โดยใช้ `argc` และ `argv` ในฟังก์ชัน `main` ใน Node.js, `process.argv` เป็นตัวเลือกหลัก มันเป็นอาร์เรย์ที่องค์ประกอบแรกคือพาธไปยังโปรแกรมทำงาน node, องค์ประกอบที่สองคือชื่อไฟล์สคริปต์, และที่เหลือคืออาร์กิวเมนต์จริงของคุณ

`yargs` เหมาะสำหรับแอพซับซ้อน: มันแยกวิเคราะห์อาร์กิวเมนต์เป็นออบเจกต์ที่เป็นประโยชน์, จัดการค่าเริ่มต้น, และแม้กระทั่งสร้างข้อความช่วยเหลือโดยอัตโนมัติ

ยังมีแพ็คเกจ `minimist` ซึ่งเป็นทางเลือกที่เบากว่า `yargs` หากคุณชอบสไตล์มินิมอล

ลึกลงไป, Node.js ใช้ `process.binding('options')` ของ V8 สำหรับการวิเคราะห์ซึ่งไม่เปิดเผยให้ผู้ใช้ทั่วไป วิธีการภายในนี้มีประโยชน์มากมายภายใน, จัดการการวิเคราะห์และการเรียกคืนตัวเลือกบรรทัดคำสั่ง

## ดูเพิ่มเติม
- เอกสารการอ้างอิง process.argv ของ Node.js: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- ที่เก็บ GitHub ของ Yargs: https://github.com/yargs/yargs
- Minimist บน npm: https://www.npmjs.com/package/minimist

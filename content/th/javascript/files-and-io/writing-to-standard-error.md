---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:26.012249-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E43\u0E19 Node.js \u0E01\u0E32\
  \u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E25\u0E07\u0E43\u0E19 stderr \u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E43\u0E0A\
  \u0E49\u0E40\u0E21\u0E18\u0E2D\u0E14 `console.error()` \u0E2B\u0E23\u0E37\u0E2D\u0E42\
  \u0E14\u0E22\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E15\u0E23\u0E07\u0E44\
  \u0E1B\u0E22\u0E31\u0E07 `process.stderr`\u2026"
lastmod: '2024-03-17T21:57:56.618568-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Node.js \u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E25\
  \u0E07\u0E43\u0E19 stderr \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E44\
  \u0E14\u0E49\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E40\u0E21\u0E18\u0E2D\u0E14 `console.error()`\
  \ \u0E2B\u0E23\u0E37\u0E2D\u0E42\u0E14\u0E22\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\
  \u0E22\u0E19\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E22\u0E31\u0E07 `process.stderr` \u0E19\
  \u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E17\
  \u0E35\u0E48\u0E41\u0E2A\u0E14\u0E07\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\
  \u0E31\u0E49\u0E07\u0E2A\u0E2D\u0E07."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19"
weight: 25
---

## วิธีทำ:
ใน Node.js การเขียนลงใน stderr สามารถทำได้โดยใช้เมธอด `console.error()` หรือโดยการเขียนตรงไปยัง `process.stderr` นี่คือตัวอย่างที่แสดงวิธีการทั้งสอง:

```javascript
// ใช้ console.error()
console.error('นี่คือข้อความแสดงผิดพลาด.');

// เขียนลงใน process.stderr โดยตรง
process.stderr.write('นี่คือข้อความแสดงผิดพลาดอีกอันหนึ่ง.\n');
```

ผลลัพธ์ตัวอย่างสำหรับทั้งสองวิธีจะปรากฏในสตรีม stderr ไม่ได้ผสมปนกับ stdout:
```
นี่คือข้อความแสดงผิดพลาด.
นี่คือข้อความแสดงผิดพลาดอีกอันหนึ่ง.
```

สำหรับการบันทึกข้อมูลที่ซับซ้อนกว่าหรือเฉพาะเจาะจงต่อแอปพลิเคชั่น โปรแกรมเมอร์ JavaScript หลายคนใช้ไลบรารีของบุคคลที่สามเช่น `winston` หรือ `bunyan` นี่คือตัวอย่างอย่างรวดเร็วโดยใช้ `winston`:

ก่อนอื่น ติดตั้ง `winston` ผ่าน npm:
```shell
npm install winston
```

จากนั้น กำหนดค่า `winston` เพื่อบันทึกข้อผิดพลาดลงใน stderr:
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// การบันทึกข้อความแสดงผิดพลาด
logger.error('ความผิดพลาดได้ถูกบันทึกผ่าน winston.');
```

การตั้งค่านี้ให้ความมั่นใจว่า เมื่อคุณบันทึกข้อผิดพลาดโดยใช้ `winston` มันจะไปที่ stderr ช่วยให้มีการแยกแยะที่ชัดเจนระหว่างผลลัพธ์มาตรฐานและข้อผิดพลาด

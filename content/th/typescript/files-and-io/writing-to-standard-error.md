---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:23.961917-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: TypeScript \u0E40\u0E1B\u0E47\u0E19\
  \u0E0A\u0E38\u0E14\u0E02\u0E22\u0E32\u0E22\u0E02\u0E2D\u0E07 JavaScript, \u0E1B\u0E23\
  \u0E30\u0E0A\u0E32\u0E04\u0E21\u0E2D\u0E32\u0E28\u0E31\u0E22\u0E2A\u0E20\u0E32\u0E1E\
  \u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\
  \u0E19 JavaScript \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\u0E0A\u0E48\u0E19 Node.js\
  \ \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E44\u0E1B\u0E22\u0E31\
  \u0E07 stderr\u2026"
lastmod: '2024-03-17T21:57:55.961746-06:00'
model: gpt-4-0125-preview
summary: "TypeScript \u0E40\u0E1B\u0E47\u0E19\u0E0A\u0E38\u0E14\u0E02\u0E22\u0E32\u0E22\
  \u0E02\u0E2D\u0E07 JavaScript, \u0E1B\u0E23\u0E30\u0E0A\u0E32\u0E04\u0E21\u0E2D\u0E32\
  \u0E28\u0E31\u0E22\u0E2A\u0E20\u0E32\u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\
  \u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19 JavaScript \u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E40\u0E0A\u0E48\u0E19 Node.js \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\
  \u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\
  \u0E32\u0E14\u0E44\u0E1B\u0E22\u0E31\u0E07 stderr \u0E15\u0E48\u0E2D\u0E44\u0E1B\
  \u0E19\u0E35\u0E49\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E17\u0E33\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19"
weight: 25
---

## วิธีทำ:
TypeScript เป็นชุดขยายของ JavaScript, ประชาคมอาศัยสภาพแวดล้อมการทำงาน JavaScript อย่างเช่น Node.js เพื่อการเขียนข้อผิดพลาดไปยัง stderr ต่อไปนี้คือวิธีสามารถทำได้โดยตรง:

```typescript
console.error("This is an error message.");
```

ตัวอย่างผลลัพธ์ไปยัง stderr:
```
This is an error message.
```

ในสภาพแวดล้อม Node.js, คุณสามารถใช้เมธอด `process.stderr.write()` สำหรับการเขียนในระดับต่ำมากขึ้น:

```typescript
process.stderr.write("Low level error message.\n");
```

ตัวอย่างผลลัพธ์ไปยัง stderr:
```
Low level error message.
```

สำหรับการบันทึกข้อผิดพลาดที่มีโครงสร้างมากขึ้น, คุณอาจใช้ไลบรารี่อื่นๆ ที่ได้รับความนิยม เช่น `winston` หรือ `pino` ต่อไปนี้คือวิธีการบันทึกข้อผิดพลาดโดยใช้ `winston`:

ก่อนอื่น, ติดตั้ง `winston`:

```bash
npm install winston
```

จากนั้นใช้มันในไฟล์ TypeScript ของคุณ:

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('Error logged using winston.');
```

สิ่งนี้จะเขียนข้อผิดพลาดทั้งในคอนโซลและไฟล์ชื่อ `error.log` ทั้งนี้, เมื่อเขียนไปยังไฟล์, สำคัญที่จะจัดการการอนุญาตไฟล์และการเปลี่ยนไฟล์เพื่อป้องกันปัญหาที่เกี่ยวข้องกับการใช้พื้นที่ดิสก์.

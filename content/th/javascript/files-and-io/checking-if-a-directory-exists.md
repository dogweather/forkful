---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:35.395409-06:00
description: "\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\
  \u0E32\u0E21\u0E35 directory \u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19 JavaScript \u0E19\
  \u0E31\u0E49\u0E19\u0E08\u0E33\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\
  \u0E1A\u0E07\u0E32\u0E19\u0E14\u0E49\u0E32\u0E19\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\
  \u0E01\u0E32\u0E23\u0E44\u0E1F\u0E25\u0E4C \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\
  \u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E15\
  \u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E01\u0E32\u0E23\u0E21\u0E35\u0E2D\u0E22\u0E39\
  \u0E48\u0E02\u0E2D\u0E07 directory\u2026"
lastmod: '2024-03-17T21:57:56.616645-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\
  \u0E32\u0E21\u0E35 directory \u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19 JavaScript \u0E19\
  \u0E31\u0E49\u0E19\u0E08\u0E33\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\
  \u0E1A\u0E07\u0E32\u0E19\u0E14\u0E49\u0E32\u0E19\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\
  \u0E01\u0E32\u0E23\u0E44\u0E1F\u0E25\u0E4C \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\
  \u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E15\
  \u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E01\u0E32\u0E23\u0E21\u0E35\u0E2D\u0E22\u0E39\
  \u0E48\u0E02\u0E2D\u0E07 directory\u2026"
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การตรวจสอบว่ามี directory อยู่ใน JavaScript นั้นจำเป็นสำหรับงานด้านการจัดการไฟล์ ช่วยให้สคริปต์สามารถตรวจสอบการมีอยู่ของ directory ก่อนที่จะอ่านหรือเขียนลงไปในนั้น การดำเนินการนี้ป้องกันข้อผิดพลาดและรับรองการดำเนินการของโปรแกรมที่ราบรื่นยิ่งขึ้น โดยเฉพาะในแอปพลิเคชั่นที่จัดการไฟล์หรือ directory อย่างไดนามิกตามข้อมูลผู้ใช้หรือแหล่งข้อมูลภายนอก

## วิธีทำ:
ใน Node.js เนื่องจาก JavaScript ไม่สามารถเข้าถึงระบบไฟล์โดยตรง โมดูล `fs` จึงโดยปกติใช้สำหรับการดำเนินการเช่นนี้ นี่คือวิธีง่ายๆในการตรวจสอบว่ามี directory อยู่โดยใช้ไฟล์ `fs.existsSync()`:

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// ตรวจสอบว่ามี directory อยู่ไหม
if (fs.existsSync(directoryPath)) {
  console.log('Directory มีอยู่.');
} else {
  console.log('Directory ไม่มีอยู่.');
}
```
**ผลลัพธ์ตัวอย่าง:**
```
Directory มีอยู่.
```
หรือสำหรับวิธีไม่บล็อกแบบแอสซิงโครนัส ใช้ `fs.promises` ด้วย `async/await`:

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('Directory มีอยู่.');
  } catch (error) {
    console.log('Directory ไม่มีอยู่.');
  }
}

checkDirectory('./sample-directory');
```
**ผลลัพธ์ตัวอย่าง:**
```
Directory มีอยู่.
```

สำหรับโปรเจคที่ใช้งานไฟล์และ directory อย่างหนัก แพ็คเกจ `fs-extra` , ซึ่งเป็นการขยายจากโมดูล `fs` ดั้งเดิม, นำเสนอวิธีการเพิ่มเติมที่สะดวก นี่คือวิธีทำเช่นเดียวกันด้วย `fs-extra`:

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// ตรวจสอบว่ามี directory อยู่ไหม
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? 'Directory มีอยู่.' : 'Directory ไม่มีอยู่.'))
  .catch(err => console.error(err));
```
**ผลลัพธ์ตัวอย่าง:**
```
Directory มีอยู่.
```

วิธีการนี้ช่วยให้โค้ดที่สะอาด อ่านง่าย และสามารถผสานรวมได้เป็นอย่างดีกับการปฏิบัติตามมาตรฐานของ JavaScript ในปัจจุบัน.

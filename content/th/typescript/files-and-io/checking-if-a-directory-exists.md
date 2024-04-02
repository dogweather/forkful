---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:40.535556-06:00
description: "\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\
  \u0E32\u0E21\u0E35\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2D\u0E22\
  \u0E39\u0E48\u0E43\u0E19 TypeScript \u0E19\u0E31\u0E49\u0E19\u0E2A\u0E33\u0E04\u0E31\
  \u0E0D\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E44\
  \u0E1F\u0E25\u0E4C \u0E40\u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\
  \u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E02\u0E35\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E44\u0E1F\u0E25\u0E4C\u2026"
lastmod: '2024-03-17T21:57:55.959971-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\
  \u0E32\u0E21\u0E35\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2D\u0E22\
  \u0E39\u0E48\u0E43\u0E19 TypeScript \u0E19\u0E31\u0E49\u0E19\u0E2A\u0E33\u0E04\u0E31\
  \u0E0D\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E44\
  \u0E1F\u0E25\u0E4C \u0E40\u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\
  \u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E02\u0E35\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E44\u0E1F\u0E25\u0E4C\u2026"
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
weight: 20
---

## อะไรและทำไม?
การตรวจสอบว่ามีไดเรกทอรีอยู่ใน TypeScript นั้นสำคัญต่อการจัดการไฟล์ เช่น การอ่านหรือเขียนข้อมูลไปยังไฟล์ การันตีว่าการดำเนินการนั้นทำได้เฉพาะกับไดเรกทอรีที่ถูกต้องเท่านั้น การดำเนินการนี้มีความสำคัญในการหลีกเลี่ยงข้อผิดพลาดที่เกิดจากการพยายามเข้าถึงหรือจัดการกับไดเรกทอรีที่ไม่มีอยู่จริง

## วิธีการ:

เมื่อ TypeScript ทำงานในสภาพแวดล้อม Node.js จะอนุญาตให้คุณตรวจสอบว่ามีไดเรกทอรีอยู่ได้โดยใช้โมดูล `fs` ซึ่งมีฟังก์ชัน `existsSync()` หรือฟังก์ชันแบบอะซิงโครนัส `access()` ควบคู่กับ `constants.F_OK`

### การใช้ `fs.existsSync()`:

```typescript
import { existsSync } from 'fs';

const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log('Directory exists.');
} else {
  console.log('Directory does not exist.');
}
```

### การใช้ `fs.access()` ควบคู่กับ `fs.constants.F_OK`:

```typescript
import { access, constants } from 'fs';

const directoryPath = './path/to/directory';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('Directory does not exist.');
    return;
  }
  console.log('Directory exists.');
});
```

**ผลลัพธ์ตัวอย่าง** สำหรับทั้งสองวิธี โดยสมมติว่าไดเรกทอรีนั้นมีอยู่จริง:
```
Directory exists.
```

และถ้าไม่มี:
```
Directory does not exist.
```

### การใช้ไลบรารีจากบุคคลที่สาม - `fs-extra`:

`fs-extra` เป็นไลบรารีจากบุคคลที่สามที่ได้รับความนิยมซึ่งเสริมฟังก์ชันก์ของโมดูล `fs` และมีฟังก์ชันที่สะดวกยิ่งขึ้น

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './path/to/directory';

pathExists(directoryPath).then(exists => {
  console.log(`Directory exists: ${exists}`);
});
```

**ผลลัพธ์ตัวอย่าง** เมื่อไดเรกทอรีนั้นมีอยู่จริง:
```
Directory exists: true
```

และถ้าไม่มี:
```
Directory exists: false
```

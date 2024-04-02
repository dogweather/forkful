---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:34.718538-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV\
  \ (Comma-Separated Values) \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E2D\u0E48\u0E32\u0E19\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\
  \u0E1F\u0E25\u0E4C CSV \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\
  \u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E1E\u0E1A\
  \u0E1A\u0E48\u0E2D\u0E22\u2026"
lastmod: '2024-03-17T21:57:55.967727-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV (Comma-Separated\
  \ Values) \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2D\u0E48\
  \u0E32\u0E19\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\
  \ CSV \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E01\u0E32\u0E23\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E1E\u0E1A\u0E1A\u0E48\u0E2D\
  \u0E22\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
weight: 37
---

## อะไร & ทำไม?

การทำงานกับ CSV (Comma-Separated Values) หมายถึงการอ่านและเขียนไฟล์ CSV ซึ่งเป็นรูปแบบการแลกเปลี่ยนข้อมูลที่พบบ่อย เนื่องจากมีความเรียบง่ายและได้รับการสนับสนุนจากหลากหลายแพลตฟอร์มและภาษา นักโปรแกรมเมอร์มักจะทำงานกับไฟล์ CSV เพื่อนำเข้าหรือส่งออกข้อมูลจากแอปพลิเคชัน, ฐานข้อมูล, และบริการต่างๆ เพื่อให้การจัดการข้อมูลและการแบ่งปันเป็นไปอย่างง่ายดาย

## วิธีการ:

ใน TypeScript, คุณสามารถทำงานกับไฟล์ CSV ผ่านโค้ดเนทีฟหรือโดยการใช้ไลบรารีของบุคคลที่สาม เช่น `csv-parser` เพื่ออ่านและ `csv-writer` เพื่อเขียนไฟล์ CSV

### การอ่าน CSV ด้วย `csv-parser`

ก่อนอื่น, ติดตั้ง `csv-parser` ผ่าน npm:

```
npm install csv-parser
```

จากนั้น, อ่านไฟล์ CSV ดังนี้:

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // ผลลัพธ์: อาร์เรย์ของอ็อบเจกต์, แต่ละตัวแทนสำหรับแถวหนึ่งในไฟล์ CSV
  });
```

โดยสมมติว่า `data.csv` มีข้อมูลดังนี้:

```
name,age
Alice,30
Bob,25
```

ผลลัพธ์จะเป็น:

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### การเขียน CSV ด้วย `csv-writer`

เพื่อเขียนไฟล์ CSV, ก่อนอื่นติดตั้ง `csv-writer`:

```
npm install csv-writer
```

จากนั้น, ใช้ดังนี้:

```typescript
import { createObjectCsvWriter as createCsvWriter } from 'csv-writer';

const csvWriter = createCsvWriter({
  path: 'out.csv',
  header: [
    {id: 'name', title: 'NAME'},
    {id: 'age', title: 'AGE'}
  ]
});

const data = [
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
];

csvWriter
  .writeRecords(data)
  .then(() => console.log('ไฟล์ CSV ได้ถูกเขียนเรียบร้อยแล้ว'));
```

โค้ดนี้เขียนข้อมูลต่อไปนี้ลงใน `out.csv`:

```
NAME,AGE
Alice,30
Bob,25
```

ตัวอย่างเหล่านี้แสดงวิธีการรวมการประมวลผล CSV ในโปรเจกต์ TypeScript ของคุณอย่างมีประสิทธิภาพ ไม่ว่าจะเป็นการอ่านข้อมูลเพื่อวิเคราะห์หรือการบันทึกข้อมูลแอปพลิเคชันภายนอก

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:43.170660-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: JavaScript \u0E44\u0E21\u0E48\
  \u0E21\u0E35\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E41\u0E22\u0E01\u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\
  \u0E2A\u0E15\u0E23\u0E34\u0E07 CSV \u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\u0E40\u0E2B\
  \u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A JSON \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\
  \u0E23\u0E01\u0E47\u0E15\u0E32\u0E21\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 CSV\
  \ \u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E14\
  \u0E32\u0E22\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49 JavaScript\u2026"
lastmod: '2024-03-17T21:57:56.624411-06:00'
model: gpt-4-0125-preview
summary: "JavaScript \u0E44\u0E21\u0E48\u0E21\u0E35\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\
  \u0E31\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E41\u0E22\u0E01\u0E2B\u0E23\u0E37\
  \u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07 CSV \u0E42\u0E14\
  \u0E22\u0E15\u0E23\u0E07\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A JSON\
  \ \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u0E01\u0E47\u0E15\u0E32\u0E21\u0E04\
  \u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 CSV \u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E07\u0E48\u0E32\u0E22\u0E14\u0E32\u0E22\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\
  \ JavaScript \u0E18\u0E23\u0E23\u0E21\u0E14\u0E32\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E07\u0E32\u0E19\u0E17\u0E35\u0E48\u0E07\u0E48\u0E32\u0E22 \u0E2B\u0E23\u0E37\u0E2D\
  \u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E48\u0E17\u0E35\u0E48\
  \u0E21\u0E35\u0E1E\u0E25\u0E31\u0E07\u0E40\u0E0A\u0E48\u0E19 `PapaParse` \u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E2A\u0E16\u0E32\u0E19\u0E01\u0E32\u0E23\u0E13\u0E4C\u0E17\
  \u0E35\u0E48\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19\u0E21\u0E32\u0E01\u0E02\u0E36\
  \u0E49\u0E19\n\n#."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
weight: 37
---

## วิธีการ:
JavaScript ไม่มีฟังก์ชันสำหรับแยกหรือสร้างสตริง CSV โดยตรงเหมือนกับ JSON อย่างไรก็ตามคุณสามารถจัดการข้อมูล CSV ได้อย่างง่ายดายโดยใช้ JavaScript ธรรมดาสำหรับงานที่ง่าย หรือใช้ไลบรารี่ที่มีพลังเช่น `PapaParse` สำหรับสถานการณ์ที่ซับซ้อนมากขึ้น

### การแยกข้อมูลพื้นฐานด้วย JavaScript ธรรมดา
เพื่อแยกสตริง CSV เป็นอาร์เรย์ของอ็อบเจ็กต์:

```javascript
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

function parseCSV(csv) {
  const lines = csv.split("\n");
  const result = [];
  const headers = lines[0].split(",");

  for (let i = 1; i < lines.length; i++) {
    const obj = {};
    const currentline = lines[i].split(",");
    
    for (let j = 0; j < headers.length; j++) {
      obj[headers[j]] = currentline[j];
    }
    result.push(obj);
  }
  
  return result;
}

console.log(parseCSV(csv));
```
ผลลัพธ์:

```
[
  { name: 'John', age: '23', city: 'New York' },
  { name: 'Jane', age: '28', city: 'Los Angeles' }
]
```

### การสร้างสตริง CSV พื้นฐานด้วย JavaScript ธรรมดา
เพื่อแปลงอาร์เรย์ของอ็อบเจ็กต์เป็นสตริง CSV:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

function arrayToCSV(arr) {
  const csv = arr.map(row => 
    Object.values(row).join(',')
  ).join('\n');
  
  return csv;
}

console.log(arrayToCSV(data));
```

ผลลัพธ์:

```
John,23,New York
Jane,28,Los Angeles
```

### ใช้ PapaParse สำหรับงาน CSV ที่ซับซ้อน
สำหรับสถานการณ์ที่ซับซ้อนมากขึ้น `PapaParse` เป็นไลบรารี่ที่เหมาะสมสำหรับการแยกและสร้างไฟล์ CSV พร้อมตัวเลือกสำหรับสตรีม, วอร์กเกอร์, และการจัดการไฟล์ขนาดใหญ่

การแยกไฟล์หรือสตริง CSV ด้วย PapaParse:

```javascript
// หลังจากเพิ่ม PapaParse ลงในโปรเจ็กต์ของคุณ
const Papa = require('papaparse');
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

Papa.parse(csv, {
  complete: function(results) {
    console.log("Parsed:", results.data);
  }
});
```

ผลิต:

```
Parsed: [
  ["name", "age", "city"],
  ["John", "23", "New York"],
  ["Jane", "28", "Los Angeles"]
]
```

การแปลงอาร์เรย์เป็นสตริง CSV ด้วย PapaParse:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

console.log(Papa.unparse(data));
```

ผลิต:

```
name,age,city
John,23,New York
Jane,28,Los Angeles
```

ตัวอย่างเหล่านี้แสดงวิธีการจัดการข้อมูล CSV พื้นฐานและขั้นสูงใน JavaScript ซึ่งทำให้การแลกเปลี่ยนข้อมูลในเว็บแอพพลิเคชั่นและอื่น ๆ ง่ายขึ้น

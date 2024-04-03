---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:59.464330-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19\u0E2A\u0E20\
  \u0E32\u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21 Node.js \u0E04\u0E38\u0E13\
  \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E42\u0E21\u0E14\u0E39\u0E25\
  \ `fs` (File System) \u0E17\u0E35\u0E48\u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\
  \u0E01\u0E31\u0E1A\u0E23\u0E30\u0E1A\u0E1A\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E02\
  \u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\
  \u2026"
lastmod: '2024-03-17T21:57:56.620587-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19\u0E2A\u0E20\u0E32\u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\
  \u0E21 Node.js \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\
  \u0E49\u0E42\u0E21\u0E14\u0E39\u0E25 `fs` (File System) \u0E17\u0E35\u0E48\u0E21\
  \u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E23\u0E30\u0E1A\u0E1A\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21 \u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E19\u0E35\u0E49\u0E41\u0E2A\u0E14\u0E07\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\
  \u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E25\u0E07\u0E43\u0E19\u0E44\u0E1F\
  \u0E25\u0E4C\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E41\u0E1A\u0E1A\
  \u0E2D\u0E30\u0E0B\u0E34\u0E07\u0E42\u0E04\u0E23\u0E19\u0E31\u0E2A."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 24
---

## วิธีการ:
ในสภาพแวดล้อม Node.js คุณสามารถใช้โมดูล `fs` (File System) ที่มาพร้อมกับระบบเพื่อเขียนไฟล์ข้อความ ตัวอย่างนี้แสดงการเขียนข้อความลงในไฟล์โดยใช้งานแบบอะซิงโครนัส:

```javascript
const fs = require('fs');

const data = 'สวัสดี, โลก! นี่คือข้อความที่จะเขียนลงในไฟล์.';

fs.writeFile('example.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('ไฟล์ถูกเขียนแล้ว.');
});
```

ผลลัพธ์ตัวอย่าง:
```
ไฟล์ถูกเขียนแล้ว.
```

สำหรับการเขียนไฟล์แบบซิงโครนัส, ใช้ `writeFileSync`:
```javascript
try {
  fs.writeFileSync('example.txt', data);
  console.log('ไฟล์ถูกเขียนแล้ว.');
} catch (error) {
  console.error('เกิดข้อผิดพลาดในการเขียนไฟล์:', error);
}
```

ในเว็บเบราว์เซอร์ยุคใหม่, File System Access API เปิดใช้งานฟีเจอร์การอ่านและเขียนไฟล์ อย่างไรก็ตาม การใช้งานนี้ได้ต้องอยู่ภายใต้การอนุญาตจากผู้ใช้ นี่คือวิธีสร้างและเขียนไฟล์:

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('สวัสดี, โลก! นี่คือการเขียนไฟล์ข้อความในเบราว์เซอร์.');
  await writable.close();
}
```

สำหรับสถานการณ์ที่ซับซ้อนขึ้น หรือเมื่อทำงานกับไฟล์ขนาดใหญ่ คุณอาจต้องการใช้ไลบรารีของบุคคลที่สาม เช่น `FileSaver.js` สำหรับเบราว์เซอร์:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["สวัสดี, โลก! นี่คือข้อความจาก FileSaver.js."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "example.txt");
</script>
```

จำไว้ว่า การเขียนไฟล์ทางด้านไคลเอนต์ (ในเบราว์เซอร์) ถูกจำกัดโดยความกังวลเรื่องความปลอดภัย และการดำเนินการใด ๆ ที่ต้องการบันทึกลงในดิสก์ท้องถิ่นของผู้ใช้ โดยทั่วไปจะต้องการอนุญาตอย่างชัดเจนจากพวกเขา.

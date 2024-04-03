---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:52.836021-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: #."
lastmod: '2024-03-17T21:57:56.623442-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
weight: 38
---

## วิธีการ:


### การแยกวิเคราะห์ JSON
ในการแปลงสตริง JSON เป็นอ็อบเจกต์ JavaScript, ใช้ `JSON.parse()`.

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // ผลลัพธ์: John
```

### การแปลงอ็อบเจกต์ JavaScript เป็นสตริง JSON
ในการแปลงอ็อบเจกต์ JavaScript กลับเป็นสตริง JSON, ใช้ `JSON.stringify()`.

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // ผลลัพธ์: {"name":"Jane","age":25,"city":"London"}
```

### การทำงานกับไฟล์ใน Node.js
เพื่ออ่านไฟล์ JSON และแปลงมันให้เป็นอ็อบเจกต์ในสภาพแวดล้อมของ Node.js, คุณสามารถใช้โมดูล `fs` ตัวอย่างนี้สมมติว่าคุณมีไฟล์ที่ชื่อว่า `data.json`.

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) throw err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

สำหรับการเขียนอ็อบเจกต์ลงในไฟล์ JSON:

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) throw err;
    console.log('ข้อมูลถูกเขียนลงในไฟล์');
});
```

### ไลบรารีของบุคคลที่สาม
สำหรับการดำเนินการ JSON ที่ซับซ้อน, เฟรมเวิร์กและไลบรารีเช่น `lodash` สามารถทำให้งานง่ายขึ้น, แต่สำหรับการดำเนินการพื้นฐาน, ฟังก์ชัน JavaScript พื้นฐานมักเพียงพอ สำหรับแอปพลิเคชันขนาดใหญ่หรือแอปพลิเคชันที่สำคัญกับประสิทธิภาพ, คุณสามารถพิจารณาไลบรารีเช่น `fast-json-stringify` สำหรับการแปลงสตริง JSON ได้เร็วขึ้นหรือ `json5` สำหรับการแยกวิเคราะห์และแปลงใช้รูปแบบ JSON ที่ยืดหยุ่นมากขึ้น

การแยกวิเคราะห์ด้วย `json5`:
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // ผลลัพธ์: John
```

ตัวอย่างเหล่านี้ครอบคลุมการดำเนินการพื้นฐานกับ JSON ใน JavaScript, สมบูรณ์สำหรับผู้เริ่มต้นที่กำลังเปลี่ยนจากภาษาอื่นและต้องการจัดการข้อมูลในแอปพลิเคชั่นเว็บอย่างมีประสิทธิภาพ.

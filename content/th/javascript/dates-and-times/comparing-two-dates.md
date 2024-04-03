---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:46.670371-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E27\u0E31\u0E15\u0E16\
  \u0E38 `Date` \u0E43\u0E19 JavaScript \u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\
  \u0E01\u0E31\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E30\u0E14\u0E27\u0E01 \u0E43\u0E19\
  \u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\u0E22\u0E1A\
  \u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32, \u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E08\
  \u0E30\u0E16\u0E39\u0E01\u0E41\u0E1B\u0E25\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E21\u0E34\
  \u0E25\u0E25\u0E34\u0E27\u0E34\u0E19\u0E32\u0E17\u0E35\u0E19\u0E31\u0E1A\u0E15\u0E31\
  \u0E49\u0E07\u0E41\u0E15\u0E48\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48 1 \u0E21\u0E01\
  \u0E23\u0E32\u0E04\u0E21 \u0E1E.\u0E28. 2513, UTC."
lastmod: '2024-03-17T21:57:56.614832-06:00'
model: gpt-4-0125-preview
summary: "\u0E27\u0E31\u0E15\u0E16\u0E38 `Date` \u0E43\u0E19 JavaScript \u0E21\u0E32\
  \u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E30\
  \u0E14\u0E27\u0E01 \u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\
  \u0E40\u0E17\u0E35\u0E22\u0E1A\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32, \u0E1E\u0E27\
  \u0E01\u0E40\u0E02\u0E32\u0E08\u0E30\u0E16\u0E39\u0E01\u0E41\u0E1B\u0E25\u0E07\u0E40\
  \u0E1B\u0E47\u0E19\u0E21\u0E34\u0E25\u0E25\u0E34\u0E27\u0E34\u0E19\u0E32\u0E17\u0E35\
  \u0E19\u0E31\u0E1A\u0E15\u0E31\u0E49\u0E07\u0E41\u0E15\u0E48\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48 1 \u0E21\u0E01\u0E23\u0E32\u0E04\u0E21 \u0E1E.\u0E28."
title: "\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\u0E22\u0E1A\u0E2A\u0E2D\
  \u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48"
weight: 27
---

## วิธีการ:
วัตถุ `Date` ใน JavaScript มาพร้อมกับความสะดวก ในการเปรียบเทียบพวกเขา, พวกเขาจะถูกแปลงเป็นมิลลิวินาทีนับตั้งแต่วันที่ 1 มกราคม พ.ศ. 2513, UTC

```javascript
let date1 = new Date('2021-07-24');
let date2 = new Date('2021-07-25');

console.log(date1 < date2); // จริง
console.log(date1 > date2); // ไม่จริง
console.log(date1.getTime() === date2.getTime()); // ไม่จริง
```

ตัวอย่างผลลัพธ์:

```
true
false
false
```

## การศึกษาลึก
ในส่วนที่ซับซ้อน, วัตถุ `Date` นั้นเป็นเพียงมิลลิวินาที ในอดีต, โปรแกรมเมอร์ต้องจัดการปฏิบัติการวันที่ด้วยตนเอง, คำนวณเวลาที่ผ่านไปจากจุดข้อมูล, บ่อยครั้งที่เสี่ยงต่อข้อผิดพลาด การเปรียบเทียบวัตถุ `Date` ทำให้ชีวิตง่ายขึ้น, ถึงแม้ยังไม่ปลอดภัยจากข้อผิดพลาด, เฉพาะกับโซนเวลาและการปรับเปลี่ยนเวลาออมแสง

มีทางเลือก? แน่นอน ไลบรารีเช่น `moment.js` หรือ `date-fns` ช่วยจัดการกับสถานการณ์ที่ซับซ้อนและให้ความสะดวกเพิ่มเติมสำหรับการจัดการวันที่

ในเรื่องของการดำเนินการ, สำคัญที่จะต้องจำไว้ว่าการเปรียบเทียบวัตถุ `Date` โดยตรง (ด้วย `==`) เป็นการเปรียบเทียบอ้างอิง, ไม่ใช่ค่า ใช้ `getTime()` เพื่อการเปรียบเทียบค่าอย่างแม่นยำ และระวังโซนเวลาเมื่อวิเคราะห์วันที่; มีโอกาสที่จะพลาดง่ายถ้าคุณไม่ระมัดระวัง

## ดูเพิ่มเติม
- เอกสารบนเว็บ MDN เกี่ยวกับ Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- ไลบรารี Moment.js: https://momentjs.com/
- ไลบรารี date-fns: https://date-fns.org/

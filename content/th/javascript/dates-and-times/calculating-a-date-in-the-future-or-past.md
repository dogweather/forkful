---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:40.138582-06:00
description: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\
  \u0E14\u0E35\u0E15\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2B\
  \u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E08\u0E33\u0E19\
  \u0E27\u0E19\u0E27\u0E31\u0E19, \u0E2A\u0E31\u0E1B\u0E14\u0E32\u0E2B\u0E4C, \u0E40\
  \u0E14\u0E37\u0E2D\u0E19, \u0E2B\u0E23\u0E37\u0E2D\u0E1B\u0E35\u0E08\u0E32\u0E01\
  \u0E08\u0E38\u0E14\u0E2B\u0E19\u0E36\u0E48\u0E07 \u0E46\u2026"
lastmod: '2024-03-17T21:57:56.615728-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\
  \u0E14\u0E35\u0E15\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2B\
  \u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E08\u0E33\u0E19\
  \u0E27\u0E19\u0E27\u0E31\u0E19, \u0E2A\u0E31\u0E1B\u0E14\u0E32\u0E2B\u0E4C, \u0E40\
  \u0E14\u0E37\u0E2D\u0E19, \u0E2B\u0E23\u0E37\u0E2D\u0E1B\u0E35\u0E08\u0E32\u0E01\
  \u0E08\u0E38\u0E14\u0E2B\u0E19\u0E36\u0E48\u0E07 \u0E46 \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E21\u0E31\u0E01\u0E15\u0E49\u0E2D\
  \u0E07\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E19\u0E35\u0E49\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E40\u0E0A\u0E48\u0E19\u0E01\u0E32\
  \u0E23\u0E15\u0E31\u0E49\u0E07\u0E27\u0E31\u0E19\u0E2B\u0E21\u0E14\u0E2D\u0E32\u0E22\
  \u0E38, \u0E01\u0E32\u0E23\u0E41\u0E08\u0E49\u0E07\u0E40\u0E15\u0E37\u0E2D\u0E19\
  , \u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E01\
  \u0E32\u0E23\u0E13\u0E4C."
title: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\u0E14\
  \u0E35\u0E15"
weight: 26
---

## อะไรและทำไม?
การคำนวณวันที่ในอนาคตหรืออดีตหมายถึงการหาวันที่เป็นจำนวนวัน, สัปดาห์, เดือน, หรือปีจากจุดหนึ่ง ๆ โปรแกรมเมอร์มักต้องการใช้งานนี้สำหรับงานเช่นการตั้งวันหมดอายุ, การแจ้งเตือน, หรือการกำหนดการณ์

## วิธีการ:
อ็อบเจกต์ `Date` ใน JavaScript คือตัวเลือกแรกสำหรับการเล่นกับวันที่ มาดูตัวอย่างกันบ้าง:

```javascript
// วันนี้
let today = new Date();
console.log(today); // แสดงผลวันที่และเวลาปัจจุบัน

// คำนวณวันที่ 7 วันในอนาคต
let nextWeek = new Date();
nextWeek.setDate(today.getDate() + 7);
console.log(nextWeek); // แสดงผลวันที่ในเวลาเดียวกัน, อีก 7 วันข้างหน้า

// คำนวณวันที่ 30 วันในอดีต
let lastMonth = new Date();
lastMonth.setDate(today.getDate() - 30);
console.log(lastMonth); // แสดงผลวันที่ในเวลาเดียวกัน, 30 วันที่ผ่านมา

// ตั้งวันที่ 1 ปีในอนาคต
let nextYear = new Date();
nextYear.setFullYear(today.getFullYear() + 1);
console.log(nextYear); // แสดงผลวันที่ในเวลาเดียวกันปีหน้า
```
ผลลัพธ์ขึ้นอยู่กับเมื่อคุณรันโค้ดนี้ เนื่องจาก `today` เป็นวันที่-เวลาของคุณในปัจจุบัน

## ลึกซึ้งยิ่งขึ้น
ก่อนที่ JavaScript จะมีฟังก์ชันการจัดการวันที่ในตัว, โปรแกรมเมอร์ต้องคำนวณวันที่ด้วยตัวเอง โดยคำนึงถึงความแตกต่างในความยาวของเดือน, ปีอธิกสุรทิน, และเขตเวลา - ซึ่งเป็นปัญหาใหญ่! ด้วย `Date`, ปัญหาส่วนใหญ่เหล่านี้จะหายไป

ทางเลือกอื่นนอกเหนือจากอ็อบเจกต์ `Date` ปกติ รวมถึงไลบรารีเช่น `moment.js` และ `date-fns`, ที่นำเสนอไวยากรณ์ที่หลากหลายขึ้นและแก้ไขจุดบกพร่องเช่นบั๊กเรื่องเวลาออมแสง.

เมื่อคำนวณวันที่, จำไว้ว่า: `Date` นับเดือนตั้งแต่ 0 (มกราคม) ถึง 11 (ธันวาคม), ไม่ใช่ 1-12 และอย่าลืมเรื่องปีอธิกสุรทินเมื่อทำงานกับวันที่ของเดือนกุมภาพันธ์

## ดูเพิ่มเติม
- MDN Web Docs เกี่ยวกับ Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- moment.js: https://momentjs.com/
- date-fns: https://date-fns.org/

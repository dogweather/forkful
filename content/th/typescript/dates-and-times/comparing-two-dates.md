---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:37.035781-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\
  \u0E22\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E2D\u0E07\u0E27\u0E31\u0E19\
  \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\
  \u0E32\u0E21\u0E2A\u0E31\u0E21\u0E1E\u0E31\u0E19\u0E18\u0E4C\u0E17\u0E32\u0E07\u0E25\
  \u0E33\u0E14\u0E31\u0E1A\u0E40\u0E27\u0E25\u0E32\u0E02\u0E2D\u0E07\u0E1E\u0E27\u0E01\
  \u0E40\u0E02\u0E32\u2014\u0E27\u0E48\u0E32\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E40\
  \u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\
  \u0E48, \u0E27\u0E31\u0E19\u0E43\u0E14\u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\
  \u0E19\u0E01\u0E48\u0E2D\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E27\u0E31\u0E19\u0E43\u0E14\
  \u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\u0E19\u0E2B\u0E25\u0E31\u0E07?\u2026"
lastmod: '2024-03-17T21:57:55.958085-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\
  \u0E22\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E2D\u0E07\u0E27\u0E31\u0E19\
  \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\
  \u0E32\u0E21\u0E2A\u0E31\u0E21\u0E1E\u0E31\u0E19\u0E18\u0E4C\u0E17\u0E32\u0E07\u0E25\
  \u0E33\u0E14\u0E31\u0E1A\u0E40\u0E27\u0E25\u0E32\u0E02\u0E2D\u0E07\u0E1E\u0E27\u0E01\
  \u0E40\u0E02\u0E32\u2014\u0E27\u0E48\u0E32\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E40\
  \u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\
  \u0E48, \u0E27\u0E31\u0E19\u0E43\u0E14\u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\
  \u0E19\u0E01\u0E48\u0E2D\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E27\u0E31\u0E19\u0E43\u0E14\
  \u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\u0E19\u0E2B\u0E25\u0E31\u0E07."
title: "\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\u0E22\u0E1A\u0E2A\u0E2D\
  \u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48"
weight: 27
---

## วิธีทำ:
ลองเปรียบเทียบวันที่กัน:

```TypeScript
const date1 = new Date('2023-04-01T00:00:00Z');
const date2 = new Date('2023-04-02T00:00:00Z');

// date1 อยู่ก่อน date2 หรือไม่?
console.log(date1 < date2); // true

// date1 เหมือนกับ date2 หรือไม่?
console.log(date1.getTime() === date2.getTime()); // false

// ห่างกันกี่วัน?
const diffTime = Math.abs(date2.getTime() - date1.getTime());
const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24)); 
console.log(diffDays); // 1
```

ผลลัพธ์ตัวอย่าง:

```
true
false
1
```

## ลึกซึ้ง
กลับไปในอดีต, วันที่มีหลายรูปแบบและการคำนวณที่ซับซ้อน. ด้วย JavaScript (และ TypeScript โดยการขยาย), วัตถุ `Date` ได้ทำให้สิ่งต่าง ๆ ง่ายขึ้น, ทำให้การจัดการเวลามีมาตรฐาน

มีทางเลือกอื่นหรือ? แน่นอน. ไลบรารีเช่น `moment.js` หรือ `date-fns` เพิ่มฟังก์ชันนาลิตี้ให้กับการจัดการวันที่ด้วยความสามารถเพิ่มเติม. แต่สำหรับการเปรียบเทียบพื้นฐาน? ความเรียบง่ายของ Date แบบเนทีฟมักจะทำงานได้ดี

ใต้ฮู้ด, `Date.getTime()` ได้มิลลิวินาทีนับตั้งแต่ยุคแรก (1 ม.ค. 1970). การเปรียบเทียบค่าเหล่านี้ช่วยกำจัดคดีของเขตเวลาและวินาทีอึด, ทำให้มันเป็นเพียงแค่ตัวเลข

## ดูเพิ่มเติม
- [Mozilla Developer Network Date Reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) สำหรับข้อมูลเชิงลึกของวัตถุ Date
- [You Don't Need Moment.js](https://github.com/you-dont-need/You-Dont-Need-Momentjs) สำหรับเวลาที่คุณอาจหรืออาจไม่ต้องการไลบรารี
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/) สำหรับข้อมูลเพิ่มเติมเกี่ยวกับพลังและปัญหาของ TypeScript

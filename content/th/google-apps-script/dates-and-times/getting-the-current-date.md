---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:48.282738-06:00
description: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\u0E43\u0E19 Google Apps Script\
  \ \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E2A\u0E14 \u0E46 \u0E0B\
  \u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E07\u0E32\u0E19\u0E17\u0E31\u0E48\u0E27\
  \u0E44\u0E1B\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\
  \u0E32\u0E19\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34, \u0E01\u0E32\
  \u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01,\u2026"
lastmod: '2024-03-17T21:57:55.729227-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\u0E43\u0E19 Google Apps Script\
  \ \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E2A\u0E14 \u0E46 \u0E0B\
  \u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E07\u0E32\u0E19\u0E17\u0E31\u0E48\u0E27\
  \u0E44\u0E1B\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\
  \u0E32\u0E19\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34, \u0E01\u0E32\
  \u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01,\u2026"
title: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\
  \u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19"
weight: 29
---

## อะไรและทำไม?

การรับวันที่ปัจจุบันใน Google Apps Script คือการดึงวันที่และเวลาสด ๆ ซึ่งเป็นงานทั่วไปสำหรับการทำงานอัตโนมัติ, การบันทึก, และการประทับเวลาในแอปที่เชื่อมโยงกับระบบนิเวศของ Google โปรแกรมเมอร์ใช้สิ่งนี้สำหรับการสร้างเนื้อหาไดนามิก, การติดตามกำหนดเส้นตาย และการวางกำหนดการภายใน Google Docs, Sheets และบริการ Google อื่น ๆ

## ขั้นตอน:

Google Apps Script ซึ่งอิงตาม JavaScript มีวิธีง่ายๆในการรับวันที่ปัจจุบัน คุณสามารถใช้ `new Date()` constructor เพื่อสร้างวัตถุวันที่ใหม่ที่แทนวันที่และเวลาปัจจุบัน นี่คือวิธีที่คุณสามารถจัดการและแสดงผลในรูปแบบต่าง ๆ

```javascript
function showCurrentDate() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // บันทึกวันที่และเวลาปัจจุบันในเขตเวลาของสคริปต์
  
  // เพื่อแสดงเฉพาะวันที่ในรูปแบบ YYYY-MM-DD
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // ตัวอย่างผลลัพธ์: "2023-04-01"
  
  // การแสดงผลในรูปแบบที่อ่านง่ายขึ้น
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // ตัวอย่างผลลัพธ์: "April 1, 2023, 12:00:00 PM GMT+1"
}
```

เหล่าสแนปเก็ตนี้แสดงวิธีการบันทึกและจัดรูปแบบวันที่และเวลาปัจจุบัน ซึ่งแสดงถึงความหลากหลายสำหรับความต้องการการเขียนโปรแกรมต่างๆ ภายใน Google Apps Script

## ลงลึก

ก่อนที่ JavaScript จะเลือกใช้วัตถุ `Date`, โปรแกรมเมอร์ต้องติดตามเวลาและวันที่ด้วยตนเองผ่านวิธีการที่ไม่มาตรฐานและยุ่งยากยิ่งขึ้น ซึ่งรวมถึงการใช้ค่าเวลาแบบรหัสเวลาและฟังก์ชั่นวันที่ที่ทำเอง ซึ่งแตกต่างกันไปในแต่ละสภาพแวดล้อมการเขียนโปรแกรม ทำให้เกิดความไม่ต่อเนื่องและปัญหาเรื่องความเข้ากันได้

การนำเสนอวัตถุ `new Date()` ใน JavaScript และโดยการขยายไปถึง Google Apps Script ได้ทำให้การดำเนินการเกี่ยวกับวันที่และเวลามาตรฐานขึ้น ทำให้ง่ายต่อการเข้าใจและลดปริมาณโค้ดที่จำเป็นสำหรับการดำเนินการที่เกี่ยวข้องกับวันที่ น่าสังเกตว่าในขณะที่การประยุกต์ใช้ของ Google Apps Script เป็นสิ่งที่สะดวกและเพียงพอสำหรับการใช้งานหลายๆอย่างภายในชุดผลิตภัณฑ์ของ Google อาจไม่รองรับทุกสถานการณ์ โดยเฉพาะอย่างยิ่งสถานการณ์ที่ต้องการการจัดการเขตเวลาที่ซับซ้อนหรือการบันทึกรหัสเวลาอย่างแม่นยำในสภาพแวดล้อมที่เร่งรีบ

สำหรับกรณีการใช้งานขั้นสูงเช่นนี้ โปรแกรมเมอร์มักจะหันไปใช้ไลบรารี เช่น Moment.js หรือ date-fns ใน JavaScript แม้ว่า Google Apps Script จะไม่รองรับไลบรารีเหล่านี้โดยตรง แต่นักพัฒนาสามารถจำลองฟังก์ชันบางอย่างของพวกเขาโดยใช้มีธอดวันที่ของ JavaScript ที่มีอยู่ หรือโดยการเข้าถึงไลบรารีภายนอกผ่าน HTML Service หรือบริการ URL Fetch ของ Apps Script ถึงแม้จะมีทางเลือกเหล่านี้ ความเรียบง่ายและการบูรณาการของฟังก์ชันวันที่และเวลาเนทีฟของ Google Apps Script ยังคงเป็นตัวเลือกหลักสำหรับงานส่วนใหญ่ในระบบนิเวศ Google

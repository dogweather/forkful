---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:31.779248-06:00
description: "\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\
  \u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 Google Apps Script,\
  \ \u0E20\u0E32\u0E29\u0E32\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E1A\u0E19\
  \u0E04\u0E25\u0E32\u0E27\u0E14\u0E4C\u0E02\u0E2D\u0E07 JavaScript \u0E17\u0E35\u0E48\
  \u0E0A\u0E48\u0E27\u0E22\u0E04\u0E38\u0E13\u0E43\u0E19\u0E01\u0E32\u0E23\u0E2D\u0E31\
  \u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34\u0E07\u0E32\u0E19\u0E15\u0E48\u0E32\u0E07\
  \u0E46 \u0E02\u0E49\u0E32\u0E21\u0E1C\u0E25\u0E34\u0E15\u0E20\u0E31\u0E13\u0E11\u0E4C\
  \u0E02\u0E2D\u0E07 Google,\u2026"
lastmod: '2024-03-17T21:57:55.706756-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\
  \u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 Google Apps Script,\
  \ \u0E20\u0E32\u0E29\u0E32\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E1A\u0E19\
  \u0E04\u0E25\u0E32\u0E27\u0E14\u0E4C\u0E02\u0E2D\u0E07 JavaScript \u0E17\u0E35\u0E48\
  \u0E0A\u0E48\u0E27\u0E22\u0E04\u0E38\u0E13\u0E43\u0E19\u0E01\u0E32\u0E23\u0E2D\u0E31\
  \u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34\u0E07\u0E32\u0E19\u0E15\u0E48\u0E32\u0E07\
  \u0E46 \u0E02\u0E49\u0E32\u0E21\u0E1C\u0E25\u0E34\u0E15\u0E20\u0E31\u0E13\u0E11\u0E4C\
  \u0E02\u0E2D\u0E07 Google,\u2026"
title: "\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\u0E02\
  \u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การหาความยาวของสตริงใน Google Apps Script, ภาษาสคริปต์บนคลาวด์ของ JavaScript ที่ช่วยคุณในการอัตโนมัติงานต่างๆ ข้ามผลิตภัณฑ์ของ Google, คือการกำหนดจำนวนตัวอักษรที่สตริงมีอยู่ โปรแกรมเมอร์มักจะทำการดำเนินการนี้เพื่อตรวจสอบข้อมูลนำเข้า, วนลูปผ่านตัวอักษร, หรือจัดการสตริงสำหรับงานการอัตโนมัติต่างๆ ภายใน Google Apps.

## วิธีการ:
ใน Google Apps Script, คุณสามารถหาความยาวของสตริงได้โดยใช้คุณสมบัติ `.length`, เหมือนกับใน JavaScript คุณสมบัตินี้จะคืนค่าจำนวนตัวอักษรภายในสตริง, รวมถึงช่องว่างและตัวละครพิเศษ นี่คือตัวอย่างบางประการ:

```javascript
// กำหนดสตริง
var text = "Hello, World!";
// หาความยาวของสตริง
var length = text.length;
// บันทึกความยาว
Logger.log(length); // ผลลัพธ์: 13
```

ในสถานการณ์ที่คุณทำงานกับข้อมูลที่ใส่โดยผู้ใช้จาก Google Forms หรือ Sheets, การหาความยาวของสตริงช่วยในการตรวจสอบข้อมูลได้:

```javascript
// ตัวอย่างข้อมูลสตริงที่ใส่โดยผู้ใช้ใน Google Sheets
var userEntry = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet().getRange("A1").getValue();
// คำนวณและบันทึกความยาวของข้อมูลที่ใส่
Logger.log(userEntry.length); // ผลลัพธ์ขึ้นอยู่กับเนื้อหาของเซลล์ A1
```

มาเพิ่มตัวอย่างที่ใช้งานได้จริงซึ่งรวมถึงเงื่อนไข หากข้อมูลนำเข้ามีความยาวเกินกำหนด, คุณอาจต้องการแสดงข้อผิดพลาดหรือคำเตือน:

```javascript
var comment = "This is a sample comment that is too long for our database.";
if(comment.length > 50) {
  Logger.log("Error: Your comment should not exceed 50 characters.");
} else {
  Logger.log("Thank you for your submission.");
}
// ผลลัพธ์: Error: Your comment should not exceed 50 characters.
```

## ลงลึกเพิ่มเติม
ในบริบทของ Google Apps Script, ที่พัฒนาบน JavaScript, คุณสมบัติ `.length` มาจากมาตรฐาน ECMAScript, ซึ่งกำกับข้อกำหนดของ JavaScript คุณสมบัติ `.length` เป็นส่วนหนึ่งของ JavaScript ตั้งแต่เริ่มแรกโดยให้วิธีง่ายๆ ในการประเมินขนาดของสตริง

รายละเอียดที่น่าสังเกตคือ Google Apps Script ถูกดำเนินการบนเซิร์ฟเวอร์ของ Google, ไม่ใช่ในเบราว์เซอร์ นี้หมายความว่าเมื่อคุณกำลังจัดการกับสตริงและความยาวของมัน, โดยเฉพาะในชุดข้อมูลขนาดใหญ่ที่ดึงมาจาก Google Sheets หรือ Docs, เวลาในการดำเนินการอาจได้รับผลกระทบเนื่องจากความล่าช้าของเครือข่ายและข้อจำกัดเวลาในการรันสคริปต์

แม้ว่า `.length` เป็นวิธีที่ตรงไปตรงมาและถูกใช้กันอย่างแพร่หลายในการหาความยาวของสตริง, กลยุทธ์อื่นๆ อาจรวมถึงการใช้ regex หรือการวนลูปผ่านสตริงเพื่อนับตัวอักษร, โดยเฉพาะเมื่อจัดการกับตัวอักษรหลายไบต์หรือเมื่อคุณต้องการกรองออกบางประเภทของตัวละคร อย่างไรก็ตาม, สำหรับวัตถุประสงค์ที่ใช้งานได้จริงภายใน Google Apps Script, `.length` นำเสนอวิธีที่เชื่อถือได้และมีประสิทธิภาพในการกำหนดความยาวของสตริง

จำไว้เสมอ, โดยเฉพาะอย่างยิ่งใน Google Apps Script, ให้คำนึงถึงบริบทที่คุณกำลังรันโค้ดของคุณ ประสิทธิภาพและข้อจำกัดในการดำเนินการอาจนำไปสู่ความจำเป็นในการปรับแต่งกระบวนการจัดการสตริงของคุณ, รวมถึงวิธีการกำหนดความยาวของมัน

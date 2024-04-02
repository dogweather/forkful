---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:35.869216-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E19 Google Apps Script (GAS) \u0E40\
  \u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E41\u0E25\u0E30\u0E14\u0E36\u0E07\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E08\
  \u0E32\u0E01\u0E44\u0E1F\u0E25\u0E4C\u0E17\u0E35\u0E48\u0E40\u0E01\u0E47\u0E1A\u0E44\
  \u0E27\u0E49\u0E43\u0E19 Google Drive \u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E35\u0E48\
  \u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E1A\u0E19\u0E04\u0E25\
  \u0E32\u0E27\u0E14\u0E4C\u0E2D\u0E37\u0E48\u0E19\u0E46\u2026"
lastmod: '2024-03-17T21:57:55.736150-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E19 Google Apps Script (GAS) \u0E40\
  \u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E41\u0E25\u0E30\u0E14\u0E36\u0E07\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E08\
  \u0E32\u0E01\u0E44\u0E1F\u0E25\u0E4C\u0E17\u0E35\u0E48\u0E40\u0E01\u0E47\u0E1A\u0E44\
  \u0E27\u0E49\u0E43\u0E19 Google Drive \u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E35\u0E48\
  \u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E1A\u0E19\u0E04\u0E25\
  \u0E32\u0E27\u0E14\u0E4C\u0E2D\u0E37\u0E48\u0E19\u0E46\u2026"
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 22
---

## อะไร & ทำไม?

การอ่านไฟล์ข้อความใน Google Apps Script (GAS) เกี่ยวข้องกับการเข้าถึงและดึงข้อมูลข้อความจากไฟล์ที่เก็บไว้ใน Google Drive หรือที่เก็บข้อมูลบนคลาวด์อื่นๆ ที่เข้าถึงได้ โปรแกรมเมอร์ต้องการอ่านไฟล์เหล่านี้เพื่อนำเข้า, จัดการ, หรือวิเคราะห์ข้อมูลข้อความโดยตรงภายในโปรเจค GAS ของพวกเขา, เปิดโอกาสให้มีการทำงานอัตโนมัติและการผสานรวมกับชุดผลิตภัณฑ์ของ Google

## วิธีทำ:

เพื่อเริ่มการอ่านไฟล์ข้อความด้วย Google Apps Script, โดยทั่วไปคุณจะต้องใช้ Google Drive API นิยามด้านล่างนี้แสดงตัวอย่างวิธีการอ่านไฟล์จาก Google Drive:

```javascript
function readFileContents(fileId) {
  // ได้รับไฟล์ Google Drive โดย ID
  var file = DriveApp.getFileById(fileId);
  
  // ดึงข้อมูล blob เป็นข้อความ
  var text = file.getBlob().getDataAsString();
  
  // บันทึกเนื้อหาไปยังบันทึกของ Google Apps Script
  Logger.log(text);
  return text;
}
```

*ผลลัพธ์ตัวอย่างในบันทึก:*

```
Hello, world! This is a test text file.
```

ในตัวอย่างนี้, `fileId` เป็นรหัสเอกลักษณ์ของไฟล์ที่คุณต้องการอ่าน. บริการ `DriveApp` จะดึงไฟล์มา, และ `getDataAsString()` จะอ่านเนื้อหาของมันเป็นสตริง คุณสามารถจัดการหรือใช้ข้อความนี้ตามที่ต้องการ.

## ไดพ์ดีพ

ในอดีต, การอ่านไฟล์ข้อความในแอปพลิเคชันเว็บ เช่น ที่สร้างด้วย Google Apps Script, เป็นเรื่องที่ท้าทายเนื่องจากข้อจำกัดด้านความปลอดภัยของเบราว์เซอร์และธรรมชาติแบบอะซิงโครนัสของ JavaScript Google Apps Script ทำให้เรื่องนี้ง่ายขึ้นด้วยบริการที่ถูกแปลงเป็นแบบสูง เช่น `DriveApp`, ให้ API ระดับสูงในการโต้ตอบกับไฟล์ของ Google Drive

อย่างไรก็ตาม, ประเด็นสำคัญคือประสิทธิภาพและข้อจำกัดเวลาการทำงานที่ Google Apps Script กำหนด, โดยเฉพาะเมื่ออ่านไฟล์ขนาดใหญ่หรือทำการดำเนินการที่ซับซ้อนกับข้อมูล ในบางกรณี, อาจมีประสิทธิภาพมากกว่าที่จะใช้บริการของ Google Cloud โดยตรงจากแบ็กเอนด์ที่ทรงพลังกว่าหรือทำการประมวลผลไฟล์ล่วงหน้าเป็นชิ้นเล็กๆ

สำหรับการประมวลผลไฟล์ที่ซับซ้อนหรือเมื่อประสิทธิภาพเวลาแบบเรียลไทม์เป็นสิ่งสำคัญ, ตัวเลือกอื่นๆ เช่น Google Cloud Functions, ซึ่งรองรับ Node.js, Python, และ Go, อาจมีความยืดหยุ่นและทรัพยากรการคำนวณมากขึ้น อย่างไรก็ตาม, สำหรับงานที่ตรงไปตรงมาภายในระบบนิเวศของ Google, โดยเฉพาะที่ความเรียบง่ายและความง่ายในการผสานรวมกับผลิตภัณฑ์ของ Google เป็นสิ่งจำเป็น, Google Apps Script ให้วิธีการที่ใช้งานง่ายอย่างน่าทึ่ง.

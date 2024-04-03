---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:17.371328-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E43\
  \u0E19 Google Apps Script \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\
  \u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E01\u0E44\u0E1B\
  \u0E22\u0E31\u0E07\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\
  \u0E27\u0E2D\u0E23\u0E4C\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01\u0E2B\u0E23\u0E37\u0E2D\
  \ API \u0E41\u0E1A\u0E1A\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E15\
  \u0E34\u0E01\u2026"
lastmod: '2024-03-17T21:57:55.712947-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E43\
  \u0E19 Google Apps Script \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\
  \u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E01\u0E44\u0E1B\
  \u0E22\u0E31\u0E07\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\
  \u0E27\u0E2D\u0E23\u0E4C\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01\u0E2B\u0E23\u0E37\u0E2D\
  \ API \u0E41\u0E1A\u0E1A\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E15\
  \u0E34\u0E01 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  \u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E14\u0E36\u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E2D\
  \u0E23\u0E4C\u0E27\u0E34\u0E2A \u0E42\u0E14\u0E22\u0E01\u0E32\u0E23\u0E1C\u0E2A\u0E32\
  \u0E19\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E41\u0E25\u0E30\
  \u0E17\u0E23\u0E31\u0E1E\u0E22\u0E32\u0E01\u0E23\u0E40\u0E27\u0E47\u0E1A\u0E17\u0E35\
  \u0E48\u0E2B\u0E25\u0E32\u0E01\u0E2B\u0E25\u0E32\u0E22\u0E40\u0E02\u0E49\u0E32\u0E01\
  \u0E31\u0E1A\u0E42\u0E1B\u0E23\u0E40\u0E08\u0E01\u0E15\u0E4C Google Apps Script\
  \ \u0E02\u0E2D\u0E07\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E42\u0E14\u0E22\u0E15\
  \u0E23\u0E07."
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## วิธีการ:
ใน Google Apps Script วิธีหลักในการส่งคำขอ HTTP คือการใช้บริการ `UrlFetchApp` บริการนี้มีวิธีการให้ทำคำขอ HTTP GET และ POST นี่เป็นตัวอย่างง่ายๆของการทำคำขอ GET เพื่อดึงข้อมูล JSON:

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

สำหรับคำขอ POST ซึ่งมักใช้ในการส่งข้อมูลไปยังเซิร์ฟเวอร์ คุณจำเป็นต้องรวมรายละเอียดเพิ่มเติมในพารามิเตอร์ตัวเลือก:

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    key1: 'value1',
    key2: 'value2'
  };
  
  var options = {
    'method' : 'post',
    'contentType': 'application/json',
    // แปลงวัตถุ JavaScript เป็นสตริง JSON
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

ตัวอย่างเหล่านี้แสดงการดำเนินการคำขอ GET และ POST พื้นฐาน ผลลัพธ์จะขึ้นอยู่กับการตอบสนองของ API และสามารถดูได้ใน Logger ของ Google Apps Script

## ศึกษาลึกลงไป
บริการ `UrlFetchApp` ของ Google Apps Script ได้พัฒนาขึ้นอย่างมากตั้งแต่เริ่มต้น โดยนำเสนอการควบคุมที่ละเอียดอ่อนยิ่งขึ้นเกี่ยวกับคำขอ HTTP ด้วยคุณสมบัติเช่นการตั้งค่าหัวข้อ ข้อมูลระบุ และการจัดการ multipart/form-data สำหรับการอัปโหลดไฟล์ แม้ว่าจะให้วิธีการที่ตรงไปตรงมาในการผสานรวมเว็บเซอร์วิสภายนอก แต่นักพัฒนาที่มาจากภาษาเบื้องหลังที่มีความสามารถมากขึ้นอาจพบว่าฟังก์ชันการทำงานมีข้อจำกัดบางประการเมื่อเปรียบเทียบกับไลบรารีเช่น `requests` ของ Python หรือ `fetch` API ใน Node.js ของ JavaScript

ข้อจำกัดหนึ่งที่น่าสังเกตคือเวลาการดำเนินการสำหรับ Google Apps Script ซึ่งส่งผลกระทบต่อคำขอที่ใช้เวลานาน นอกจากนี้ ในขณะที่ `UrlFetchApp` ครอบคลุมช่วงการใช้งานที่กว้าง สถานการณ์ที่ซับซ้อนยิ่งขึ้นที่เกี่ยวข้องกับการรับรองความถูกต้องของ OAuth หรือการจัดการข้อมูลที่มีขนาดใหญ่มากอาจต้องการวิธีการสร้างสรรค์หรือใช้ทรัพยากร Google Cloud เพิ่มเติม

อย่างไรก็ตาม สำหรับการผสานรวมส่วนใหญ่ที่นักพัฒนา Google Workspace พบเจอ - ตั้งแต่การเรียกข้อมูลโดยอัตโนมัติไปจนถึงการโพสต์อัปเดตไปยังบริการภายนอก - `UrlFetchApp` ให้เครื่องมือที่มีประสิทธิภาพและเข้าถึงได้ง่าย การผสานรวมเข้ากับ Google Apps Script หมายความว่าไม่จำเป็นต้องใช้ไลบรารีภายนอกหรือการตั้งค่าที่ซับซ้อน ทำให้การดำเนินการคำขอ HTTP นั้นทำได้ค่อนข้างง่ายในข้อจำกัดของ Google Apps Script ในขณะที่ภูมิทัศน์ของเว็บ API ยังคงขยายตัว `UrlFetchApp` ยังคงเป็นสะพานสำคัญสำหรับโปรแกรม Google Apps Script เพื่อโต้ตอบกับโลกภายนอกอีโคซิสเต็มของ Google

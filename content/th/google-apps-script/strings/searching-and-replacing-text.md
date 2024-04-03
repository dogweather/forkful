---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:46.331466-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Google Apps Script \u0E19\
  \u0E33\u0E40\u0E2A\u0E19\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E15\u0E23\
  \u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32\u0E43\u0E19\u0E01\u0E32\u0E23\u0E04\
  \u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\u0E19\u0E17\u0E35\u0E48\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21, \u0E42\u0E14\u0E22\u0E40\u0E09\u0E1E\u0E32\
  \u0E30\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E22\u0E34\u0E48\u0E07\u0E20\u0E32\u0E22\u0E43\
  \u0E19 Google Docs \u0E41\u0E25\u0E30 Sheets\u2026"
lastmod: '2024-03-17T21:57:55.700605-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u0E19\u0E33\u0E40\u0E2A\u0E19\u0E2D\u0E27\u0E34\u0E18\
  \u0E35\u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32\
  \u0E43\u0E19\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\
  \u0E17\u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21, \u0E42\
  \u0E14\u0E22\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E22\u0E34\
  \u0E48\u0E07\u0E20\u0E32\u0E22\u0E43\u0E19 Google Docs \u0E41\u0E25\u0E30 Sheets\
  \ \u0E14\u0E49\u0E32\u0E19\u0E25\u0E48\u0E32\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\
  \u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E17\
  \u0E31\u0E49\u0E07\u0E2A\u0E2D\u0E07\n\n#."
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 10
---

## วิธีการ:
Google Apps Script นำเสนอวิธีที่ตรงไปตรงมาในการค้นหาและแทนที่ข้อความ, โดยเฉพาะอย่างยิ่งภายใน Google Docs และ Sheets ด้านล่างเป็นตัวอย่างสำหรับทั้งสอง

### Google Docs:
เพื่อค้นหาและแทนที่ข้อความในเอกสาร Google, คุณจะต้องโต้ตอบกับคลาส `DocumentApp` หลัก

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // เพื่อค้นหาและแทนที่วลีเฉพาะ
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// วิธีการใช้
searchReplaceInDoc();
```

โค้ดชิ้นนี้ค้นหาเหตุการณ์ทั้งหมดของ `'searchText'` ในเอกสาร Google ที่ใช้งานอยู่และแทนที่ด้วย `'replacementText'`.

### Google Sheets:
ในลักษณะเดียวกัน, ใน Google Sheets, คุณสามารถใช้ `SpreadsheetApp` เพื่อทำการค้นหาและแทนที่:

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // ค้นหาและแทนที่ในแผ่นงานที่ใช้งานอยู่ในปัจจุบัน
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// วิธีการใช้
searchReplaceInSheet();
```

ในตัวอย่างนี้, `createTextFinder('searchText')` ค้นหาในแผ่นงานที่ใช้งานอยู่สำหรับ 'searchText', และ `replaceAllWith('replacementText')` แทนที่เหตุการณ์ทั้งหมดด้วย 'replacementText'.

## ดำดิ่งลึก
ฟังก์ชันค้นหาและแทนที่ใน Google Apps Script ได้รับอิทธิพลอย่างมากจากลักษณะเว็บของมัน, ทำให้สคริปต์สามารถจัดการข้อความต่างๆ ข้าม Google Apps ได้อย่างราบรื่น ในแง่ประวัติศาสตร์, ความสามารถนี้มาจากบริบทที่กว้างขึ้นของการประมวลผลและการจัดการข้อความในการเขียนโปรแกรม, ที่ regular expressions และฟังก์ชันสตริงในภาษาเช่น Perl และ Python สร้างมาตรฐานสูงสำหรับความยืดหยุ่นและความสามารถ

แม้ว่าฟังก์ชันค้นหาและแทนที่ใน Google Apps Script จะมีพลังมากสำหรับการแทนที่ที่ตรงไปตรงมา, แต่ยังขาดความสามารถของ regular expression แบบเต็มที่พบในภาษาอื่นๆ ตัวอย่างเช่น, แม้ว่าคุณสามารถใช้ regular expressions พื้นฐานใน `createTextFinder` ใน Google Sheets, ตัวเลือกสำหรับการจับคู่แพทเทิร์นที่ซับซ้อนและการจัดการนั้นจำกัดเมื่อเทียบกับ Perl หรือ Python

สำหรับความต้องการการประมวลผลข้อความที่ซับซ้อนกว่านี้, โปรแกรมเมอร์อาจต้องส่งออกเนื้อหา Google Docs หรือ Sheets เป็นรูปแบบที่สามารถประมวลผลได้ภายนอกด้วยภาษาที่มีความสามารถมากขึ้นหรือใช้ Google Apps Script เรียก API หรือบริการภายนอกที่เสนอความสามารถในการจัดการข้อความที่ซับซ้อนกว่า

แม้จะมีข้อจำกัดเหล่านี้, สำหรับงานค้นหาและแทนที่ทั่วไปภายในระบบนิเวศของ Google Apps, Google Apps Script นำเสนอโซลูชั่นที่ง่าย, มีประสิทธิภาพ, และสามารถรวมได้อย่างสูงที่ปรับให้เหมาะสมกับความต้องการของการอัตโนมัติและการเขียนสคริปต์ภายในชุดเครื่องมือการผลิตของ Google

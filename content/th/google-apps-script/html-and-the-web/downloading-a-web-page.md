---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:46.607452-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Google Apps\
  \ Script, \u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23 `UrlFetchApp` \u0E40\u0E1B\u0E47\u0E19\
  \u0E01\u0E38\u0E0D\u0E41\u0E08\u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E43\u0E19\u0E01\u0E32\
  \u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E40\u0E19\u0E37\u0E49\
  \u0E2D\u0E2B\u0E32\u0E40\u0E27\u0E47\u0E1A \u0E14\u0E49\u0E32\u0E19\u0E25\u0E48\u0E32\
  \u0E07\u0E19\u0E35\u0E49\u0E04\u0E37\u0E2D\u0E04\u0E39\u0E48\u0E21\u0E37\u0E2D\u0E17\
  \u0E35\u0E25\u0E30\u0E02\u0E31\u0E49\u0E19\u0E15\u0E2D\u0E19\u0E41\u0E25\u0E30\u0E15\
  \u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E46\u2026"
lastmod: '2024-03-17T21:57:55.715224-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Google Apps Script, \u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23 `UrlFetchApp`\
  \ \u0E40\u0E1B\u0E47\u0E19\u0E01\u0E38\u0E0D\u0E41\u0E08\u0E2A\u0E33\u0E04\u0E31\
  \u0E0D\u0E43\u0E19\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\
  \u0E14\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E40\u0E27\u0E47\u0E1A \u0E14\u0E49\
  \u0E32\u0E19\u0E25\u0E48\u0E32\u0E07\u0E19\u0E35\u0E49\u0E04\u0E37\u0E2D\u0E04\u0E39\
  \u0E48\u0E21\u0E37\u0E2D\u0E17\u0E35\u0E25\u0E30\u0E02\u0E31\u0E49\u0E19\u0E15\u0E2D\
  \u0E19\u0E41\u0E25\u0E30\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\
  \u0E32\u0E22\u0E46 \u0E17\u0E35\u0E48\u0E41\u0E2A\u0E14\u0E07\u0E27\u0E34\u0E18\u0E35\
  \u0E14\u0E36\u0E07\u0E41\u0E25\u0E30\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E40\u0E19\
  \u0E37\u0E49\u0E2D\u0E2B\u0E32 HTML \u0E02\u0E2D\u0E07\u0E2B\u0E19\u0E49\u0E32\u0E40\
  \u0E27\u0E47\u0E1A."
title: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A"
weight: 42
---

## วิธีการ:
ใน Google Apps Script, บริการ `UrlFetchApp` เป็นกุญแจสำคัญในการดาวน์โหลดเนื้อหาเว็บ ด้านล่างนี้คือคู่มือทีละขั้นตอนและตัวอย่างง่ายๆ ที่แสดงวิธีดึงและบันทึกเนื้อหา HTML ของหน้าเว็บ:

1. **การดำเนินการ Fetch พื้นฐาน:**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- โค้ดนี้ดึงเนื้อหา HTML ของ example.com และบันทึกมันลง log เป็นการสาธิตที่ตรงไปตรงมาในการเข้าถึงแหล่งข้อมูลของเว็บเพจโดยไม่มีพารามิเตอร์เพิ่มเติม

2. **การจัดการกับการ Redirect และ HTTPS:**

สำหรับ HTTPS หรือการจัดการกับ redirects, โค้ดยังคงคล้ายกันมาก, แต่พิจารณาการใช้งานการจัดการข้อผิดพลาดหรือตัวเลือกเฉพาะสำหรับ redirects:

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // ติดตาม redirects โดยอัตโนมัติ
    'muteHttpExceptions': true // ปิดเสียงข้อยกเว้นที่เป็นไปได้เพื่อจัดการกับมันอย่างสง่างาม
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **ข้อจำกัดด้าน Rate Limits และ Quotas:**

ต้องระวังโควต้าของ Google Apps Script; การใช้งานหนักอาจต้องมีการจัดการข้อผิดพลาดสำหรับข้อจำกัดด้านอัตราการใช้งาน

## การศึกษาลึก
ในอดีต, การดาวน์โหลดและจัดการเนื้อหาเว็บเริ่มต้นด้วยการร้องขอ HTTP แบบง่าย, และกลายเป็นสิ่งที่พัฒนาอย่างมากกับการมาถึงของภาษาสคริปต์ Google Apps Script ทำให้การดำเนินการเช่นนี้ภายในระบบนิเวศ G Suite เป็นสิ่งที่ง่ายดาย, ใช้ประโยชน์จากโครงสร้างพื้นฐานที่แข็งแกร่งของ Google บริการ `UrlFetchApp` เป็นส่วนกลางของฟังก์ชันนี้, อำนวยความสะดวกในการร้องขอ HTTP/S ที่ซับซ้อนเป็นอินเตอร์เฟซระดับแอพพลิเคชันที่ง่ายขึ้น

แม้ว่าจะสะดวก, แต่ Google Apps Script อาจไม่เสมอไปเป็นเครื่องมือที่ดีที่สุดสำหรับการคัดลอกข้อมูลมากหนักๆ หรือเมื่อต้องการการประมวลผลข้อมูลหลังการดาวน์โหลดที่ซับซ้อนเนื่องจากมีการจำกัดเวลาการทำงานและโควต้าของ Google ในกรณีเช่นนั้น, กรอบการทำงานการคัดลอกข้อมูลเฉพาะหรือภาษาที่ออกแบบมาสำหรับการดำเนินการ I/O แบบอื่นๆ เช่น Node.js กับไลบรารีอย่าง Puppeteer หรือ Cheerio อาจเสนอความยืดหยุ่นและพลังการทำงานมากขึ้น

นอกจากนี้, ในขณะที่ Google Apps Script เป็นเครื่องมือที่ยอดเยี่ยมสำหรับการรวมกับบริการ Google (เช่น Sheets, Docs และ Drive) และดำเนินการดึงข้อมูลที่เบาบาง, มันสำคัญที่จะต้องจำข้อจำกัดของสภาพแวดล้อมการดำเนินการไว้ สำหรับงานที่หนักหน่วง, ให้พิจารณาใช้ Google Cloud Functions หรือบริการขั้นสูงของ Apps Script กับทรัพยากรคอมพิวเตอร์ภายนอกสำหรับการประมวลผล

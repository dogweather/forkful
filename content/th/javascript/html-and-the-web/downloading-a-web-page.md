---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:21.748478-06:00
description: "\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E40\u0E27\u0E47\
  \u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\
  \u0E40\u0E23\u0E35\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 HTML, CSS, JavaScript\
  \ \u0E41\u0E25\u0E30\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E37\u0E48\u0E19\
  \u0E46 \u0E17\u0E35\u0E48\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E36\u0E49\u0E19\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A\u0E08\u0E32\u0E01\u0E40\u0E0B\u0E34\u0E23\
  \u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.600351-06:00'
model: gpt-4-0125-preview
summary: "\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E40\u0E27\u0E47\
  \u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\
  \u0E40\u0E23\u0E35\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 HTML, CSS, JavaScript\
  \ \u0E41\u0E25\u0E30\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E37\u0E48\u0E19\
  \u0E46 \u0E17\u0E35\u0E48\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E36\u0E49\u0E19\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A\u0E08\u0E32\u0E01\u0E40\u0E0B\u0E34\u0E23\
  \u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\u0E4C\u2026"
title: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A"
---

{{< edit_this_page >}}

## อะไรและทำไม?

ดาวน์โหลดเว็บเพจหมายถึงการเรียกข้อมูล HTML, CSS, JavaScript และข้อมูลอื่นๆ ที่สร้างขึ้นหน้าเว็บจากเซิร์ฟเวอร์ โปรแกรมเมอร์ทำเช่นนี้เพื่อการวิเคราะห์เนื้อหา, การทำงานอัตโนมัติกับการโต้ตอบ, หรือการเก็บถาวรหน้าเว็บ

## วิธีการ:

นี่คือวิธีง่ายๆในการดาวน์โหลดหน้าเว็บโดยใช้ Node.js กับ `node-fetch`:

```Javascript
const fetch = require('node-fetch'); // คุณอาจจำเป็นต้องติดตั้งสิ่งนี้ก่อน!

async function downloadPage(url) {
    try {
        const response = await fetch(url);
        const body = await response.text();
        console.log(body); // แสดงผล HTML ของหน้าเพจ
    } catch (error) {
        console.error(error);
    }
}

downloadPage('https://example.com');
```

ตัวอย่างผลลัพธ์:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## ศึกษาลึก

ในอดีต, การดาวน์โหลดหน้าเว็บทำได้โดยใช้ XMLHTTPRequest ในเบราว์เซอร์หรือโมดูล `http` ใน Node.js อย่างไรก็ตาม, หลังจาก ES6, `fetch` API กลายเป็นมาตรฐานสมัยใหม่เนื่องจากมีไวยากรณ์ที่ง่ายกว่าและมีธรรมชาติที่พื้นฐานเป็นพรอมิส

ทางเลือกอื่น ๆ ได้แก่ `axios`, แพ็กเกจ npm ที่ได้รับความนิยม ซึ่งจัดการคำขอด้วยฟังก์ชันที่มากกว่า fetch ดั้งเดิม เพื่อกรณีการใช้งานที่ซับซ้อน, คุณอาจใช้ `puppeteer` เพื่อจัดการแสดงหน้าเว็บในเบราว์เซอร์ที่ไม่มีหัว, ซึ่งมีประโยชน์สำหรับการจัดการกับเนื้อหาที่แสดงผลโดย JavaScript

เมื่อดำเนินการดาวน์โหลดหน้าเว็บ, จำเป็นที่จะต้องให้ความสนใจกับประเด็นเช่น การเคารพ `robots.txt`, การจัดการ `User-Agent` เพื่อหลีกเลี่ยงการถูกบล็อก, และการจัดการการประมวลผลอย่างไม่ต่อเนื่องอย่างระมัดระวังเพื่อหลีกเลี่ยงปัญหาที่อาจเกิดขึ้นกับการโหลดเซิร์ฟเวอร์หรือสถานการณ์การแข่งขัน

## ดูเพิ่มเติม

- คู่มือ MDN Web Docs สำหรับ `fetch` API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
- หน้า GitHub ของ Axios: https://github.com/axios/axios
- หน้า GitHub ของ Puppeteer: https://github.com/puppeteer/puppeteer
- บทความเกี่ยวกับหลักการดีที่สุดในการเว็บสกรีป: https://www.scrapingbee.com/blog/web-scraping-best-practices/

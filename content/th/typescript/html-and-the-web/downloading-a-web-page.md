---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:47.498426-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E04\u0E38\u0E13\u0E2A\
  \u0E32\u0E21\u0E32\u0E23\u0E16\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E43\u0E19 TypeScript \u0E42\u0E14\u0E22\
  \u0E43\u0E0A\u0E49 Node.js \u0E41\u0E25\u0E30\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\
  \u0E35 `node-fetch` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\
  \u0E01\u0E32\u0E23."
lastmod: '2024-03-17T21:57:55.942776-06:00'
model: gpt-4-0125-preview
summary: "\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E14\u0E32\u0E27\
  \u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E43\
  \u0E19 TypeScript \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49 Node.js \u0E41\u0E25\u0E30\
  \u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35 `node-fetch` \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23."
title: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A"
weight: 42
---

## วิธีการ:
คุณสามารถดาวน์โหลดเว็บเพจใน TypeScript โดยใช้ Node.js และไลบรารี `node-fetch` นี่คือวิธีการ:

```TypeScript
import fetch from 'node-fetch';

async function downloadWebPage(url: string): Promise<void> {
    try {
        const response = await fetch(url);
        const body = await response.text();
        console.log(body); // สิ่งนี้จะแสดงเนื้อหา HTML ไปยังคอนโซล
    } catch (error) {
        console.error('การดาวน์โหลดล้มเหลว:', error);
    }
}

// ใช้ฟังก์ชั่น
downloadWebPage('https://example.com');
```

ตัวอย่างผลลัพธ์ (ตัดทอน):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## ลงลึกมากขึ้น
ในอดีต เนื้อหาเว็บถูกดาวน์โหลดผ่านเครื่องมือเช่น `wget` หรือ `curl` ในสภาพแวดล้อม command-line อย่างไรก็ตาม ในการเขียนโปรแกรมสมัยใหม่ เรามีไลบรารี เช่น `node-fetch`, `axios`, หรือ `request` (ถูกลดการใช้งานแต่ยังคงใช้งานอยู่) ที่มีฟังก์ชันเพิ่มเติมและง่ายต่อการผสานเข้ากับแอพพลิเคชัน JavaScript/TypeScript ของเรา

เมื่อดาวน์โหลดเว็บเพจ มีมากกว่าเพียง HTML เท่านั้น CSS, JavaScript, ภาพและสินทรัพย์อื่นๆก็เป็นส่วนหนึ่งของข้อตกลง โดยปกติแล้ว HTML จะถูกเรียกใช้เป็นอันดับแรกและจากนั้นการประมวลผลหรือการดาวน์โหลดเพิ่มเติมจะขึ้นอยู่กับความต้องการจากหน้าเว็บ

ในแง่ของการดำเนินการ, `node-fetch` นั้นใช้ API window.fetch สำหรับ Node.js โดยมันจะส่งคืนคำสัญญาที่ตอบสนองต่อการตอบกลับของคำขอ, ช่วยให้คุณสามารถได้รับสตรีมข้อความ (.text()), วัตถุ JSON (.json()), หรือแม้กระทั่งบัฟเฟอร์ (.buffer()) สำหรับข้อมูลทวิภาค

โปรดจำไว้ว่าสิทธิในการเก็บข้อมูลเว็บถูกกำหนดโดยไฟล์ `robots.txt` ของเว็บไซต์และเงื่อนไขการให้บริการ ตรวจสอบเสมอว่าคุณได้รับอนุญาตให้เก็บข้อมูลจากไซต์นั้นๆ และคำนึงถึงข้อจำกัดเรื่องอัตราการเข้าเยี่ยมชมเพื่อหลีกเลี่ยงปัญหาทางกฎหมายหรือการถูกบล็อก IP ของคุณ

## ดูเพิ่มเติมได้ที่
- [เอกสาร `node-fetch`](https://github.com/node-fetch/node-fetch)
- [MDN Web Docs เกี่ยวกับ Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [ไลบรารี `axios`](https://github.com/axios/axios)
- [รหัสสถานะ HTTP (เพื่อจัดการการตอบสนอง)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)
- [ความถูกต้องของการเก็บข้อมูลเว็บ](https://benbernardblog.com/web-scraping-and-crawling-are-perfectly-legal-right/)

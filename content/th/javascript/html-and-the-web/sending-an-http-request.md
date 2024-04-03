---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:59.754584-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E40\
  \u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E42\u0E04\u0E49\u0E14\
  \ JavaScript \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E2A\u0E37\u0E48\u0E2D\u0E2A\u0E32\
  \u0E23\u0E01\u0E31\u0E1A\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\
  \u0E4C \u0E40\u0E1B\u0E47\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25, \u0E14\u0E36\u0E07\u0E17\u0E23\u0E31\u0E1E\u0E22\u0E32\u0E01\
  \u0E23,\u2026"
lastmod: '2024-03-17T21:57:56.598576-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E40\
  \u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E42\u0E04\u0E49\u0E14\
  \ JavaScript \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E2A\u0E37\u0E48\u0E2D\u0E2A\u0E32\
  \u0E23\u0E01\u0E31\u0E1A\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\
  \u0E4C \u0E40\u0E1B\u0E47\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25, \u0E14\u0E36\u0E07\u0E17\u0E23\u0E31\u0E1E\u0E22\u0E32\u0E01\
  \u0E23, \u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\
  \u0E2D\u0E23\u0E4C\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\
  \u0E01\u0E32\u0E23."
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## อะไร & ทำไม?

การส่งคำขอ HTTP เป็นวิธีที่โค้ด JavaScript ของคุณสื่อสารกับเซิร์ฟเวอร์ เป็นการทำเพื่อแลกเปลี่ยนข้อมูล, ดึงทรัพยากร, หรือส่งข้อมูลไปยังเซิร์ฟเวอร์เพื่อดำเนินการ

## วิธีการ:

JavaScript ใช้ API `fetch` ในการส่งคำขอ นี่คือวิธีการทำคำขอ GET ง่ายๆ:

```javascript
fetch('https://jsonplaceholder.typicode.com/posts/1')
  .then(response => response.json())
  .then(json => console.log(json))
  .catch(err => console.error('Error:', err));
```

ผลลัพธ์จะเป็นข้อมูล JSON จาก URL ง่ายไหมล่ะ?

และสำหรับคำขอ POST:

```javascript
fetch('https://jsonplaceholder.typicode.com/posts', {
  method: 'POST',
  body: JSON.stringify({
    title: 'foo',
    body: 'bar',
    userId: 1,
  }),
  headers: {
    'Content-type': 'application/json; charset=UTF-8',
  },
})
  .then(response => response.json())
  .then(json => console.log(json))
  .catch(err => console.error('Error:', err));
```

นี่คือการส่งข้อมูลใหม่และแสดงผลตอบกลับจากเซิร์ฟเวอร์

## ลงลึก

คำขอ HTTP มีมาตั้งแต่เริ่มต้นของเว็บ—คิดถึงฟอร์ม HTML XMLHttpRequest (XHR) เคยเป็นวิธีหลักในการส่งคำขอใน JavaScript, แต่มันไม่คล่องตัว

เข้าสู่ `fetch`, วิธีการทันสมัยที่ใช้พื้นฐานของ promise ทำให้มันสะอาดและแข็งแกร่งขึ้น ไม่เหมือนกับ XHR, `fetch` จัดการทั้งคำขอและการตอบกลับใน API เดียวที่เป็นระเบียบและถูกตั้งไว้ในภาษาเอง, ไม่ต้องการไลบรารี่

ทางเลือก? แน่นอน ไลบรารี่เช่น Axios หรือ Ajax ของ jQuery ยังคงถูกใช้งาน พวกเขาเสนอโครงสร้างประโยคและวิธีแก้ปัญหาสำหรับปัญหาเฉพาะ, แต่ `fetch` เป็นฟังก์ชันดั้งเดิมและโดยทั่วไปเป็นทิศทางที่ควรไป

รายละเอียดในการใช้งาน? จำไว้ว่าต้องจัดการกับข้อบกพร่อง, ทำงานกับประเภทการตอบกลับที่แตกต่างกัน, และตระหนักถึงกฎของการแบ่งปันทรัพยากรข้ามแหล่งที่มา (CORS)
    
## ดูเพิ่มเติมที่

- MDN เอกสาร API `fetch`: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- การใช้ promises ใน JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises
- เรียนรู้เกี่ยวกับ CORS: https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS

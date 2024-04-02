---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:22.670641-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E1E\
  \u0E23\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\
  \u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E08\u0E30\u0E2B\u0E21\u0E32\u0E22\u0E16\
  \u0E36\u0E07\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\u0E0A\u0E37\u0E48\u0E2D\u0E1C\u0E39\
  \u0E49\u0E43\u0E0A\u0E49\u0E41\u0E25\u0E30\u0E23\u0E2B\u0E31\u0E2A\u0E1C\u0E48\u0E32\
  \u0E19\u0E17\u0E35\u0E48\u0E16\u0E39\u0E01\u0E40\u0E02\u0E49\u0E32\u0E23\u0E2B\u0E31\
  \u0E2A\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A base64 \u0E20\u0E32\u0E22\
  \u0E43\u0E19\u0E2A\u0E48\u0E27\u0E19\u0E2B\u0E31\u0E27\u0E02\u0E2D\u0E07\u0E04\u0E33\
  \u0E02\u0E2D\u2026"
lastmod: '2024-03-17T21:57:56.601227-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E1E\
  \u0E23\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\
  \u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E08\u0E30\u0E2B\u0E21\u0E32\u0E22\u0E16\
  \u0E36\u0E07\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\u0E0A\u0E37\u0E48\u0E2D\u0E1C\u0E39\
  \u0E49\u0E43\u0E0A\u0E49\u0E41\u0E25\u0E30\u0E23\u0E2B\u0E31\u0E2A\u0E1C\u0E48\u0E32\
  \u0E19\u0E17\u0E35\u0E48\u0E16\u0E39\u0E01\u0E40\u0E02\u0E49\u0E32\u0E23\u0E2B\u0E31\
  \u0E2A\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A base64 \u0E20\u0E32\u0E22\
  \u0E43\u0E19\u0E2A\u0E48\u0E27\u0E19\u0E2B\u0E31\u0E27\u0E02\u0E2D\u0E07\u0E04\u0E33\
  \u0E02\u0E2D\u2026"
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
weight: 45
---

## อะไรและทำไม?

การส่งคำขอ HTTP พร้อมการตรวจสอบพื้นฐานจะหมายถึงการรวมชื่อผู้ใช้และรหัสผ่านที่ถูกเข้ารหัสในรูปแบบ base64 ภายในส่วนหัวของคำขอ นักพัฒนาทำเช่นนี้เพื่อเข้าถึงทรัพยากรที่ต้องการรูปแบบการตรวจสอบที่ง่ายเพื่อรับรองระดับหนึ่งของความปลอดภัย

## วิธีการ:

นี่คือตัวอย่างการใช้ Fetch API ของ JavaScript:

```javascript
const url = 'https://some-protected-resource.com/data';
const username = 'YourUsername';
const password = 'YourPassword';

const headers = new Headers();
headers.set('Authorization', 'Basic ' + btoa(username + ':' + password));

fetch(url, { method: 'GET', headers: headers })
  .then(response => {
    if (response.ok) return response.json();
    throw new Error('Network response was not ok.');
  })
  .then(data => console.log(data))
  .catch(error => console.error('Fetch error: ', error));
```

ตัวอย่างผลลัพธ์ (พิมพ์ลงในคอนโซล):

```javascript
{
  "protected": "data",
  "moreData": 12345
}
```

## ศึกษาเพิ่มเติม

ก่อนที่จะดำดิ่งลงไปอย่างลึกซึ้ง มาทำความเข้าใจบริบทกันสักหน่อย การตรวจสอบพื้นฐานเป็นหนึ่งในรูปแบบการรักษาความปลอดภัยของเว็บเซอร์วิสที่ง่ายที่สุด โดยจะส่งข้อมูลประจำตัวในส่วนหัวไปพร้อมทุกคำขอ

บริบททางประวัติศาสตร์:
- การตรวจสอบพื้นฐานของ HTTP เป็นวิธีโบราณที่ได้รับการบรรยายครั้งแรกใน RFC 7617 จากปี 2015 ซึ่งได้มาแทนที่ RFC 2617 ที่เก่ากว่าอีกในปี 1999
- วิธีนี้ถูกใช้กันอย่างแพร่หลายเนื่องจากความง่าย แต่จะไม่ปลอดภัยถ้าไม่ใช้ HTTPS เนื่องจากการเข้ารหัส base64 สามารถถอดรหัสได้อย่างง่ายดาย

ทางเลือก:
- OAuth: มาตรฐานที่มีความปลอดภัยและซับซ้อนมากขึ้นสำหรับการมอบหมายการเข้าถึง ใช้ในกรณีที่คุณต้องการให้การเข้าถึงโดยไม่แชร์ข้อมูลรหัสผ่าน
- API Keys: โทเค็นเดียวที่ง่ายต่อการจัดการกว่าโปรโตคอล OAuth ที่ซับซ้อน
- Bearer Tokens: โดยเฉพาะ JWT (JSON Web Tokens) ซึ่งสามารถบรรจุข้อมูลได้มากกว่า

รายละเอียดในการนำไปใช้:
- การเข้ารหัส Base64 ทำการแปลงสตริง username:password เป็นลำดับของอักขระที่สามารถส่งผ่านได้กว้างขวางมากขึ้น
- ต้องแน่ใจเสมอว่าการเชื่อมต่อคือ HTTPS เพื่อป้องกันข้อมูลประจำตัวจากการถูกดักจับ
- การพัฒนาสมัยใหม่ให้ความสำคัญกับโทเค็นและคุกกี้เซสชั่นสำหรับการตรวจสอบความถูกต้อง เนื่องจากมีความปลอดภัยและหลากหลายมากขึ้น

## ดูเพิ่มเติม

- [Mozilla Developer Network - การตรวจสอบอนุญาต](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [RFC 7617 - HTTP การตรวจสอบพื้นฐาน](https://tools.ietf.org/html/rfc7617)
- [บทนำสู่ OAuth 2.0](https://www.digitalocean.com/community/tutorials/an-introduction-to-oauth-2)
- [JSON Web Tokens (JWT)](https://jwt.io/introduction/)

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:26.930188-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E15\u0E31\u0E27\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C."
lastmod: '2024-04-05T21:54:01.455495-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
weight: 45
---

## วิธีการ:
```typescript
import axios from 'axios';

// การเข้ารหัสชื่อผู้ใช้และรหัสผ่านของคุณ
const token = Buffer.from('ชื่อผู้ใช้ของคุณ:รหัสผ่านของคุณ').toString('base64');
const url = 'https://your.api/endpoint';

// ตั้งค่าคำขอ HTTP ด้วย Axios
axios.get(url, {
  headers: {
    'Authorization': `Basic ${token}`
  }
})
.then(response => {
  console.log(response.data); // นี่คือผลลัพธ์ที่คุณคาดหวัง
})
.catch(error => {
  console.error("โอ้โห, เกิดข้อผิดพลาด!", error);
});
```

ตัวอย่างผลลัพธ์:

```
{ "message": "คุณเข้าสู่ระบบแล้ว! ยินดีต้อนรับสู่แดน API ลับ." }
```

## การศึกษาลึกลงไป
ในอดีต, ก่อนที่ OAuth และ JWTs จะเข้ามายึดครองฉาก, การยืนยันตัวตนแบบพื้นฐานเป็นตัวเลือกแรกๆ มันยังคงมีประโยชน์สำหรับเครื่องมือภายในหรือ Proof of Concepts (PoCs) ความคิดคือง่ายดาย: เพิ่มส่วนหัวที่มี 'Authorization', ใช้ 'Basic ' + ตัวเข้ารหัส base64 'ชื่อผู้ใช้:รหัสผ่าน' วาว, คุณผ่านประตูเข้าไป

แต่มันไม่ใช่ทั้งหมดที่ดูดี มีความเสี่ยง - ถ้าคุณไม่ใช้ HTTPS, คุณกำลังโห่ร้องข้อมูลรับรองของคุณออกมาอย่างเปิดเผย ทางเลือก? โทเค็น OAuth2, JWTs, คีย์ API - พวกมันเหมือนกับตัวเลือกที่แข็งแกร่งและเงียบกว่า พวกมันให้บริการเพื่อวัตถุประสงค์ที่คล้ายกันแต่มีความซับซ้อนและความปลอดภัยมากขึ้น

เมื่อใช้การยืนยันตัวตนแบบพื้นฐานใน TypeScript, ตัวเลือกทั่วไปคือ `axios` หรือ `fetch` ในกรณีของเรา, `axios` ทำให้การตั้งค่าส่วนหัวที่กำหนดเองเป็นเรื่องง่าย นอกจากนี้, มันคืนค่า promises, ทำให้มันเป็นฝันกับ `async/await`

จำไว้: 'Basic' จะเริ่มเผยให้เห็นอายุของมันในเว็บสมัยใหม่ที่ HTTPS เป็นสิ่งจำเป็นและมาตรฐานความปลอดภัยสูงขึ้น แต่, สำหรับเครือข่ายภายในหรือที่ความปลอดภัยระดับสูงไม่ได้เป็นสิ่งจำเป็น, มันเป็นเรื่องง่าย

## ดูเพิ่มเติม
สำหรับวิธีการยืนยันตัวตนและแนวปฏิบัติที่ดีที่สุดด้านความปลอดภัย:

- [MDN Web Docs: Authorization](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [OWASP Authentication Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html)
- เอกสารทางการ `axios` สำหรับส่วนหัว HTTP ที่กำหนดเอง: [เอกสาร Axios](https://axios-http.com/docs/req_config)

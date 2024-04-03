---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:22.685046-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E21\u0E32\u0E25\u0E07\
  \u0E21\u0E37\u0E2D\u0E17\u0E33\u0E01\u0E31\u0E1A\u0E42\u0E04\u0E49\u0E14\u0E1A\u0E49\
  \u0E32\u0E07 \u0E27\u0E34\u0E18\u0E35\u0E19\u0E35\u0E49\u0E08\u0E30\u0E43\u0E0A\u0E49\
  \ `curl` \u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\u0E37\u0E2D\u0E1A\u0E23\
  \u0E23\u0E17\u0E31\u0E14\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E17\u0E35\u0E48\u0E1E\
  \u0E1A\u0E1A\u0E48\u0E2D\u0E22 \u0E43\u0E2B\u0E49\u0E41\u0E17\u0E19\u0E17\u0E35\u0E48\
  \ `username:password` \u0E14\u0E49\u0E27\u0E22\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E1B\u0E23\u0E30\u0E08\u0E33\u0E15\u0E31\u0E27\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\
  \ \u0E41\u0E25\u0E30\u2026"
lastmod: '2024-03-17T21:57:56.397162-06:00'
model: gpt-4-0125-preview
summary: "\u0E21\u0E32\u0E25\u0E07\u0E21\u0E37\u0E2D\u0E17\u0E33\u0E01\u0E31\u0E1A\
  \u0E42\u0E04\u0E49\u0E14\u0E1A\u0E49\u0E32\u0E07 \u0E27\u0E34\u0E18\u0E35\u0E19\u0E35\
  \u0E49\u0E08\u0E30\u0E43\u0E0A\u0E49 `curl` \u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\
  \u0E07\u0E21\u0E37\u0E2D\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\u0E33\u0E2A\u0E31\
  \u0E48\u0E07\u0E17\u0E35\u0E48\u0E1E\u0E1A\u0E1A\u0E48\u0E2D\u0E22 \u0E43\u0E2B\u0E49\
  \u0E41\u0E17\u0E19\u0E17\u0E35\u0E48 `username:password` \u0E14\u0E49\u0E27\u0E22\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E1B\u0E23\u0E30\u0E08\u0E33\u0E15\u0E31\u0E27\
  \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E41\u0E25\u0E30 `http://example.com/resource`\
  \ \u0E14\u0E49\u0E27\u0E22 URL \u0E40\u0E1B\u0E49\u0E32\u0E2B\u0E21\u0E32\u0E22\u0E02\
  \u0E2D\u0E07\u0E04\u0E38\u0E13."
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
weight: 45
---

## วิธีการ:
มาลงมือทำกับโค้ดบ้าง วิธีนี้จะใช้ `curl` เครื่องมือบรรทัดคำสั่งที่พบบ่อย ให้แทนที่ `username:password` ด้วยข้อมูลประจำตัวของคุณ และ `http://example.com/resource` ด้วย URL เป้าหมายของคุณ

```Bash
curl -u username:password http://example.com/resource
```

หรือนำข้อมูลประจำตัวไปเข้ารหัสใน base64 ล่วงหน้าแล้วใช้งานดังนี้:

```Bash
# เข้ารหัสข้อมูลประจำตัว
credentials=$(echo -n username:password | base64)

# ส่งคำขอ
curl -H "Authorization: Basic $credentials" http://example.com/resource
```

ตัวอย่างผลลัพธ์สำหรับคำขอที่สำเร็จอาจจะดูเหมือนนี้:

```Bash
{
  "data": "ข้อมูลที่ถูกจำกัด",
  "message": "อนุญาตให้เข้าถึง"
}
```

## ลงลึกมากขึ้น
โดยประวัติศาสตร์แล้ว การพิสูจน์ตัวตนแบบพื้นฐานได้เป็นส่วนหนึ่งของ HTTP ตั้งแต่วันแรกๆ แต่ไม่ได้หมายความว่าจะไม่มีข้อบกพร่อง - ประการสำคัญคือความเปราะบางถ้าไม่ได้ใช้ผ่านช่องทางที่ปลอดภัย เช่น HTTPS

ตัวเลือกอื่น ๆ ได้แก่ OAuth ซึ่งปลอดภัยกว่าและให้การควบคุมที่ละเอียดกว่าเกี่ยวกับสิ่งที่ถูกเข้าถึง Digest authentication อีกตัวเลือกหนึ่ง ซึ่งส่งข้อมูลประจำตัวที่ถูกแฮชแทนที่จะเป็นข้อความธรรมดา

สำหรับกลไก ตอนคุณส่งข้อมูลประจำตัวแบบ basic auth พวกมันจะถูกรวมไว้ในหัวข้อ HTTP โดยถูกเข้ารหัสใน Base64 ไม่ใช่การเข้ารหัส ดังนั้นหากคุณไม่ได้ใช้ HTTPS ใครก็ตามที่ดักฟังคำขอสามารถถอดรหัสได้อย่างง่ายดาย การใช้ HTTPS ช่วยปลอดภัยต่อการส่งข้อมูล โดยเข้ารหัสทุกอย่างระหว่างไคลเอ็นต์และเซิร์ฟเวอร์

## ดูเพิ่มเติม
- เอกสารอย่างเป็นทางการของ cURL: https://curl.haxx.se/docs/manpage.html
- HTTP Authentication: Basic and Digest Access Authentication (IETF RFC 7617): https://tools.ietf.org/html/rfc7617
- บทนำสู่ OAuth: https://oauth.net/2/introduction/

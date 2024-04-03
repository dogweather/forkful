---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:59.508778-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Fish Shell,\
  \ \u0E43\u0E0A\u0E49 `curl` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E04\u0E33\
  \u0E02\u0E2D HTTP \u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\
  \u0E1A\u0E2A\u0E34\u0E17\u0E18\u0E34\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19 \u0E40\
  \u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19 `username`, `password`, \u0E41\u0E25\u0E30\
  \ `the_url`."
lastmod: '2024-03-17T21:57:56.645139-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Fish Shell, \u0E43\u0E0A\u0E49 `curl` \u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E17\u0E33\u0E04\u0E33\u0E02\u0E2D HTTP \u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E15\
  \u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\u0E17\u0E18\u0E34\u0E1E\u0E37\u0E49\
  \u0E19\u0E10\u0E32\u0E19 \u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19 `username`,\
  \ `password`, \u0E41\u0E25\u0E30 `the_url`."
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E1E\u0E23\
  \u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E23\u0E2D\u0E07\u0E04\u0E27\
  \u0E32\u0E21\u0E16\u0E39\u0E01\u0E15\u0E49\u0E2D\u0E07\u0E41\u0E1A\u0E1A\u0E1E\u0E37\
  \u0E49\u0E19\u0E10\u0E32\u0E19"
weight: 45
---

## วิธีการ:
ใน Fish Shell, ใช้ `curl` เพื่อทำคำขอ HTTP พร้อมตรวจสอบสิทธิพื้นฐาน เปลี่ยน `username`, `password`, และ `the_url`:

```Fish Shell
set -x AUTH (echo -n "username:password" | base64)
curl -H "Authorization: Basic $AUTH" the_url
```

หรือ, ให้ `curl` จัดการการเข้ารหัส:

```Fish Shell
curl -u username:password the_url
```

ตัวอย่างผลลัพธ์อาจดูเช่นนี้:

```Fish Shell
HTTP/1.1 200 OK
Content-Type: application/json
{
  "message": "การตรวจสอบสิทธิ์สำเร็จ."
}
```

## การดำน้ำลึก
การตรวจสอบสิทธิพื้นฐาน (basic authentication) เป็นส่วนหนึ่งของโปรโตคอล HTTP, มีมาตั้งแต่ต้นยุค 90 แม้จะใช้งานง่าย แต่มันไม่ค่อยปลอดภัยเนื่องจากรหัสผ่านถูกเข้ารหัสด้วย base64 เท่านั้น ไม่ได้เข้ารหัสแบบเต็มรูปแบบ HTTPS ช่วยได้บ้าง แต่ไม่ใช่วิธีที่สมบูรณ์แบบ

ทางเลือกอื่น ๆ รวมถึง OAuth, ซึ่งใช้โทเค็นแทนข้อมูลประจำตัว, เพิ่มชั้นความปลอดภัย สำหรับความปลอดภัยเพิ่มเติม, พิจารณาใช้ API key หรือ JWT (JSON Web Tokens)

ด้วย Fish Shell, เรากำลังใช้ `curl`, เครื่องมือที่ทรงพลังซึ่งรองรับโปรโตคอลและวิธีการพิสูจน์ตัวตนที่หลากหลาย ตัวเลือก `-u` สะดวก แต่ควรหลีกเลี่ยงการแข็งรหัสลับเอาไว้; แทนที่จะใช้ตัวแปรแวดล้อมหรือไฟล์คอนฟิกพร้อมสิทธิ์การเข้าถึงที่เหมาะสม

## ดูเพิ่มเติม:
- เอกสาร cURL: https://curl.se/docs/httpscripting.html
- RFC ของ HTTP Basic Auth: https://tools.ietf.org/html/rfc7617
- เอกสาร Fish Shell: https://fishshell.com/docs/current/index.html
- การเข้าใจ JWT: https://jwt.io/introduction/

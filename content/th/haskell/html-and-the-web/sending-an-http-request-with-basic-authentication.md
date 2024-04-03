---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:24.804625-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\u0E33: \u0E04\u0E38\
  \u0E13\u0E08\u0E30\u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\u0E41\u0E1E\u0E47\u0E04\
  \u0E40\u0E01\u0E08 `http-conduit` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A HTTP \u0E41\u0E25\u0E30 `base64-bytestring`\
  \ \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\
  \u0E23\u0E2B\u0E31\u0E2A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E23\u0E31\u0E1A\u0E23\
  \u0E2D\u0E07 \u0E19\u0E33\u0E40\u0E02\u0E49\u0E32\u0E21\u0E32\u0E41\u0E25\u0E30\u0E43\
  \u0E0A\u0E49 `applyBasicAuth`\u2026"
lastmod: '2024-03-17T21:57:56.267874-06:00'
model: gpt-4-0125-preview
summary: "\u0E04\u0E38\u0E13\u0E08\u0E30\u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\
  \u0E41\u0E1E\u0E47\u0E04\u0E40\u0E01\u0E08 `http-conduit` \u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A HTTP\
  \ \u0E41\u0E25\u0E30 `base64-bytestring` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E23\u0E2B\u0E31\u0E2A\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E23\u0E31\u0E1A\u0E23\u0E2D\u0E07 \u0E19\u0E33\u0E40\u0E02\u0E49\u0E32\
  \u0E21\u0E32\u0E41\u0E25\u0E30\u0E43\u0E0A\u0E49 `applyBasicAuth` \u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E40\u0E1E\u0E34\u0E48\u0E21\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E23\
  \u0E31\u0E1A\u0E23\u0E2D\u0E07\u0E25\u0E07\u0E43\u0E19\u0E04\u0E33\u0E02\u0E2D\u0E02\
  \u0E2D\u0E07\u0E04\u0E38\u0E13."
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
weight: 45
---

## วิธีการทำ:
คุณจะต้องใช้แพ็คเกจ `http-conduit` สำหรับการทำงานกับ HTTP และ `base64-bytestring` สำหรับการเข้ารหัสข้อมูลรับรอง นำเข้ามาและใช้ `applyBasicAuth` เพื่อเพิ่มข้อมูลรับรองลงในคำขอของคุณ

```Haskell
import Network.HTTP.Simple
import Data.ByteString.Char8 (pack)
import Data.ByteString.Base64 (encode)

-- สร้างส่วนหัวการรับรองความถูกต้องแบบพื้นฐาน
let username = "user"
let password = "pass"
let auth = encode $ pack (username ++ ":" ++ password)

-- สร้างคำขอของคุณ
request' = parseRequest_ "GET http://example.com/secret"
let request = setRequestHeader "Authorization" ["Basic " <> auth] request'

-- ดำเนินการคำขอ
response <- httpLBS request

-- จัดการกับการตอบกลับ
print $ getResponseBody response
```

นี่จะแสดงผลการตอบกลับจาก API หากข้อมูลรับรองของคุณถูกต้อง

## การศึกษาอย่างลึกซึ้ง
การรับรองความถูกต้องแบบพื้นฐานนั้นเก่าแก่ในโลกเว็บ ถูกออกแบบในยุค 90 และมันง่ายมาก ๆ: `username:password` ที่ถูกเข้ารหัสด้วย base64 และส่งในส่วนหัว มันขาดคุณสมบัติที่ทันสมัย เช่น เวลาหมดอายุของโทเค็น และเนื่องจากไม่ได้เข้ารหัส จึงควรถูกใช้งานผ่าน HTTPS เท่านั้น

ทางเลือกอย่าง OAuth ให้การควบคุมที่มีความปลอดภัยและแยบยลมากขึ้น สำหรับ Haskell แล้ว ไลบรารีเช่น `http-client` และ `wreq` ให้ตัวเลือกและความยืดหยุ่นมากขึ้น

ในแง่ของการทำให้ใช้งานได้จริง จำไว้ว่าอย่าเขียนข้อมูลรับรองแบบฝังตรง! ใช้ตัวแปรแวดล้อมหรือตู้นิรภัยที่ปลอดภัยในการผลิต และเนื่องจากการเข้ารหัส `base64` ไม่ใช่การเข้ารหัส (ใครก็สามารถถอดรหัสได้) การใช้ HTTPS จึงไม่เพียงแต่เป็นความคิดที่ดีเท่านั้น แต่เป็นสิ่งที่จำเป็น

## ดูเพิ่มเติม
- เอกสาร Haskell `http-conduit`: https://hackage.haskell.org/package/http-conduit
- `base64-bytestring` สำหรับการเข้ารหัส: https://hackage.haskell.org/package/base64-bytestring
- สำหรับความปลอดภัยที่เข้มงวด อ่านเกี่ยวกับ OAuth2 ใน Haskell: https://hackage.haskell.org/package/hoauth2
- อ่านเพิ่มเติมเกี่ยวกับแนวทางปฏิบัติที่ดีที่สุดในการเก็บข้อมูลลับ: https://www.yesodweb.com/book/security-considerations

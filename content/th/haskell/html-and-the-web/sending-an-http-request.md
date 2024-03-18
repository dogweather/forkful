---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:49.535192-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E04\
  \u0E37\u0E2D\u0E01\u0E32\u0E23\u0E02\u0E2D\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2B\
  \u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E01\u0E23\u0E30\u0E17\u0E33\u0E08\u0E32\u0E01\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\
  \u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\
  \u0E34\u0E14\u0E15\u0E48\u0E2D\u0E01\u0E31\u0E1A API, \u0E14\u0E36\u0E07\u0E40\u0E19\
  \u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E40\u0E27\u0E47\u0E1A, \u0E2B\u0E23\u0E37\u0E2D\
  \u0E2A\u0E37\u0E48\u0E2D\u0E2A\u0E32\u0E23\u0E23\u0E30\u0E2B\u0E27\u0E48\u0E32\u0E07\
  \u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E15\u0E48\u0E32\u0E07\u0E46"
lastmod: '2024-03-17T21:57:56.265001-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E04\
  \u0E37\u0E2D\u0E01\u0E32\u0E23\u0E02\u0E2D\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2B\
  \u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E01\u0E23\u0E30\u0E17\u0E33\u0E08\u0E32\u0E01\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\
  \u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\
  \u0E34\u0E14\u0E15\u0E48\u0E2D\u0E01\u0E31\u0E1A API, \u0E14\u0E36\u0E07\u0E40\u0E19\
  \u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E40\u0E27\u0E47\u0E1A, \u0E2B\u0E23\u0E37\u0E2D\
  \u0E2A\u0E37\u0E48\u0E2D\u0E2A\u0E32\u0E23\u0E23\u0E30\u0E2B\u0E27\u0E48\u0E32\u0E07\
  \u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E15\u0E48\u0E32\u0E07\u0E46"
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การส่งคำขอ HTTP คือการขอข้อมูลหรือการกระทำจากเว็บเซิร์ฟเวอร์ โปรแกรมเมอร์ทำเช่นนี้เพื่อติดต่อกับ API, ดึงเนื้อหาเว็บ, หรือสื่อสารระหว่างบริการต่างๆ

## วิธีการ:
ลองมาทำสิ่งที่สนุกๆ กัน เราต้องการแพคเกจ `http-client` และ `http-client-tls` ตั้งค่า stack ของคุณและเพิ่มมันไปยังไฟล์ `package.yaml` หรือ `.cabal` ของคุณ จากนั้น ใช้คำสั่ง `stack build` หรือคำสั่งที่เหมาะสมเพื่อดึงมันมา

นี่คือตัวอย่างคำขอ GET ง่ายๆ:

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "http://httpbin.org/get"
    response <- httpLbs request manager
    L8.putStrLn $ responseBody response
```

สิ่งนี้จะพิมพ์ JSON ที่คุณได้รับจาก `httpbin.org`

## ลงลึก
ย้อนกลับไปในวันที่ การส่งคำขอ HTTP ของ Haskell เป็นเรื่องที่ไม่ตรงไปตรงมาเท่าไหร่ แต่ไลบรารีเช่น `http-client` ได้ทำให้กระบวนการนี้ง่ายขึ้น

มีทางเลือกอื่นไหม? แน่นอน มี `wreq`, `req` และอื่นๆ บ่อยครั้งที่มาพร้อมกับ syntactic sugar หรือคุณสมบัติเพิ่มเติม แต่ `http-client` เหมือนเป็นมีดสวิสที่เชื่อถือได้ในลิ้นชักของคุณ – มันเสมอทำงานให้เสร็จ

ในส่วนภายใน, `http-client` ใช้ `Manager` เพื่อจัดการการเชื่อมต่อ มันมีประสิทธิภาพและใช้งาน socket ซ้ำ คุณสามารถปรับแต่งมันได้ แต่ค่าเริ่มต้นก็เพียงพอแล้วเพื่อเริ่มต้น

## ดูเพิ่มเติม
เพื่อขยายชุดเครื่องมือของคุณ ลองดูสิ่งเหล่านี้:

- [แพคเกจ `http-client`](https://www.stackage.org/package/http-client)
- [แพคเกจ `wreq` สำหรับการเข้าหาที่ทันสมัยกว่า](https://www.stackage.org/package/wreq)
- [Hackage สำหรับไลบรารีของ Haskell](https://hackage.haskell.org/)

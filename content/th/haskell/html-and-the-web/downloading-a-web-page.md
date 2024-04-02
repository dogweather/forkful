---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:09.672193-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\
  \u0E32\u0E01\u0E2D\u0E34\u0E19\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E40\u0E19\u0E47\u0E15\
  ; \u0E21\u0E31\u0E19\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E2A\u0E33\u0E40\u0E19\u0E32\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E2D\u0E48\u0E32\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E1B\u0E23\
  \u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E43\u0E19\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\
  \u0E07. \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  \u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
lastmod: '2024-03-17T21:57:56.266928-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\
  \u0E32\u0E01\u0E2D\u0E34\u0E19\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E40\u0E19\u0E47\u0E15\
  ; \u0E21\u0E31\u0E19\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E2A\u0E33\u0E40\u0E19\u0E32\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E2D\u0E48\u0E32\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E1B\u0E23\
  \u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E43\u0E19\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\
  \u0E07. \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  \u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
title: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A"
weight: 42
---

## อะไร & ทำไม?

การดาวน์โหลดเว็บเพจหมายถึงการเก็บข้อมูลจากอินเทอร์เน็ต; มันเหมือนกับการบันทึกสำเนาเพื่ออ่านหรือประมวลผลในเครื่อง. โปรแกรมเมอร์ทำเช่นนี้เพื่อดึงข้อมูล, โต้ตอบกับบริการเว็บ, หรือสำรองเว็บไซต์.

## วิธีการ:

ลองมาดูตัวอย่างง่ายๆ โดยใช้ไลบรารี `http-conduit` ของ Haskell เป็นอันดับแรก, ติดตั้งมันโดยใช้ `cabal install http-conduit` จากนั้น:

```Haskell
import Network.HTTP.Conduit -- ไลบรารีเครือข่ายหลัก
import qualified Data.ByteString.Lazy as L -- เราจะต้องใช้ Lazy ByteStrings

-- ฟังก์ชั่นสำหรับการดาวน์โหลดเว็บเพจ
downloadPage :: String -> IO L.ByteString
downloadPage url = simpleHttp url

main :: IO ()
main = do
    -- ใช้ฟังก์ชันเพื่อดาวน์โหลดหน้าเว็บ
    content <- downloadPage "http://example.com"
    -- ทำอะไรบางอย่างกับเนื้อหา, เช่น พิมพ์ออกมา
    L.putStr content
```

เมื่อรัน, คุณจะเห็น HTML ของ `http://example.com` บนหน้าจอของคุณ.

## ค้นลึก

การร้องขอ HTTP ใน Haskell ไม่เคยเป็นเรื่องง่ายเสมอไป. ไลบรารีเก่าๆ เช่น `HTTP` ต้องการโค้ดพื้นฐานมากกว่า. ด้วย `http-conduit`, ความซับซ้อนนั้นถูกทำให้เรียบง่าย.

มีวิธีอื่นๆ ที่มีอยู่, เช่น คำสั่ง `wget` ในสคริปต์เชลล์ หรือ ไลบรารี `requests` ของ Python. แต่เหล่านี้อาจไม่มีประสิทธิภาพหรือเข้ากับบริบทแบบฟังก์ชันของ Haskell ได้ดีเสมอไป.

ใต้ฮู้ด, `http-conduit` ใช้ Manager เพื่อจัดการกับการรวมการเชื่อมต่อและ Keep-Alive สำหรับ HTTP1.1, ทำให้มันมีประสิทธิภาพมากขึ้นสำหรับคำขอหลายๆ อัน.

## ดูเพิ่มเติม

- สำหรับการใช้งาน `http-conduit` ในระดับสูงขึ้น: [http-conduit ที่ Hackage](https://hackage.haskell.org/package/http-conduit)
- เพื่อทำความเข้าใจเกี่ยวกับ ByteString: [ByteString ที่ Hackage](https://hackage.haskell.org/package/bytestring)

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:26.108821-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E40\u0E23\u0E35\u0E22\u0E01\
  \u0E43\u0E0A\u0E49\u0E42\u0E14\u0E22\u0E2A\u0E48\u0E07 \"world\" \u0E40\u0E1B\u0E47\
  \u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C."
lastmod: '2024-04-05T21:54:01.994690-06:00'
model: gpt-4-0125-preview
summary: "\u0E40\u0E23\u0E35\u0E22\u0E01\u0E43\u0E0A\u0E49\u0E42\u0E14\u0E22\u0E2A\
  \u0E48\u0E07 \"world\" \u0E40\u0E1B\u0E47\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C."
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
weight: 23
---

## วิธีทำ:
```haskell
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hello, " ++ show args ++ "!")
```

เรียกใช้โดยส่ง "world" เป็นอาร์กิวเมนต์:

```bash
$ runhaskell yourprogram.hs world
Hello, ["world"]!
```

## ลงลึก
Haskell เป็นภาษาที่เรียบร้อย มีรากฐานในยุค 80 นิยมความบริสุทธิ์และการพิมพ์แบบสแตติก มันได้มีวิธีจัดการอาร์กิวเมนต์บนคอมมานด์ไลน์ตั้งแต่ยุคแรกๆ ในภาษาอื่นอาจเป็นเรื่องที่ค่อนข้างกระบวนการ แต่ที่นี่ เราพูดถึงโมนาด IO เพื่อจัดการกับโลกภายนอกที่ไม่แน่นอน

ทางเลือกอื่น? คุณสามารถใช้ไลบรารีอย่าง `optparse-applicative` สำหรับงานที่ซับซ้อน แต่สำหรับกรณีง่ายๆ `getArgs` ก็ทำงานได้ดี

ใต้ฮูด? `getArgs` เป็นฟังก์ชันที่ดำดิ่งไปในระบบของคุณ ค้นหาสิ่งก่อนหน้าชื่อโปรแกรมในเทอร์มินัล และส่งคืนรายการของสตริงให้คุณ มันถูกติดตั้งในไลบรารีฐานของ Haskell พึ่งพาฟังก์ชันระดับต่ำในภาษา C เพื่อทำงานหนัก ดีใช่ไหม?

## ดูเพิ่มเติม
- ขุดลึกกับ `getArgs`: [Hoogle on System.Environment](https://hoogle.haskell.org/?hoogle=System.Environment.getArgs)
- ก้าวขึ้นในการวิเคราะห์อาร์กิวเมนต์: [optparse-applicative on Hackage](https://hackage.haskell.org/package/optparse-applicative)

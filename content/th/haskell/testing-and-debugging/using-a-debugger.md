---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:47.611528-06:00
description: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E15\u0E31\u0E27\
  \u0E14\u0E35\u0E1A\u0E31\u0E01 (debugger) \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E14\u0E33\u0E14\u0E34\u0E48\u0E07\u0E40\u0E02\u0E49\u0E32\u0E44\
  \u0E1B\u0E43\u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E14\
  \u0E49\u0E27\u0E22\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\u0E37\u0E2D\u0E17\
  \u0E35\u0E48\u0E2D\u0E2D\u0E01\u0E41\u0E1A\u0E1A\u0E21\u0E32\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A, \u0E2B\u0E22\u0E38\u0E14\u0E0A\
  \u0E31\u0E48\u0E27\u0E04\u0E23\u0E32\u0E27, \u0E41\u0E25\u0E30\u0E41\u0E01\u0E49\
  \u0E44\u0E02\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E02\u0E13\u0E30\u0E17\u0E35\
  \u0E48\u0E01\u0E33\u0E25\u0E31\u0E07\u0E17\u0E33\u0E07\u0E32\u0E19\u0E2D\u0E22\u0E39\
  \u0E48\u2026"
lastmod: '2024-03-17T21:57:56.272670-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E15\u0E31\u0E27\
  \u0E14\u0E35\u0E1A\u0E31\u0E01 (debugger) \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E14\u0E33\u0E14\u0E34\u0E48\u0E07\u0E40\u0E02\u0E49\u0E32\u0E44\
  \u0E1B\u0E43\u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E14\
  \u0E49\u0E27\u0E22\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\u0E37\u0E2D\u0E17\
  \u0E35\u0E48\u0E2D\u0E2D\u0E01\u0E41\u0E1A\u0E1A\u0E21\u0E32\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A, \u0E2B\u0E22\u0E38\u0E14\u0E0A\
  \u0E31\u0E48\u0E27\u0E04\u0E23\u0E32\u0E27, \u0E41\u0E25\u0E30\u0E41\u0E01\u0E49\
  \u0E44\u0E02\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E02\u0E13\u0E30\u0E17\u0E35\
  \u0E48\u0E01\u0E33\u0E25\u0E31\u0E07\u0E17\u0E33\u0E07\u0E32\u0E19\u0E2D\u0E22\u0E39\
  \u0E48\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
---

## วิธีการ:
มาเดินเล่นกับ GHCi, สภาพแวดล้อมแบบโต้ตอบของ Haskell ที่สามารถทำหน้าที่เป็นตัวดีบักพื้นฐานได้ คุณเริ่มต้นมันขึ้นมาพร้อมโค้ด Haskell ของคุณแล้วเริ่มค้นหา นี่คือตัวอย่าง:

```Haskell
main :: IO ()
main = do
    putStrLn "เฮ้, คุณชื่ออะไร?"
    name <- getLine
    putStrLn $ "สวัสดี, " ++ name ++ "! มาดีบักกัน."
    let result = buggyFunction 5
    print result

buggyFunction :: Int -> Int
buggyFunction n = n * 2 -- ทำเป็นว่ามีบั๊กอยู่ที่นี่
```

เริ่มต้นการดีบักกับ GHCi:

```bash
$ ghci YourHaskellFile.hs
```

ตั้งจุดหยุดที่ `buggyFunction`:

```Haskell
Prelude> :break buggyFunction
```

เรียกใช้โปรแกรมของคุณ:

```Haskell
Prelude> :main
เฮ้, คุณชื่ออะไร?
```

โปรแกรมของคุณหยุดที่ `buggyFunction` ตอนนี้คุณสามารถตรวจสอบตัวแปร, เดินผ่านโค้ด, และประเมินนิพจน์ได้

## ลงลึก:
โดยประวัติศาสตร์แล้ว ชื่อเสียงของ Haskell ว่ามีฟังก์ชันบริสุทธิ์และระบบการพิมพ์ที่แข็งแกร่งทำให้มีความเชื่อว่าเครื่องมือดีบักมีความสำคัญน้อยลง ความจริงแตกต่างออกไป—โปรแกรมที่ซับซ้อนเสมอได้ประโยชน์จากเครื่องมือดีบักที่ดี GHCi ให้คำสั่งดีบักพื้นฐาน อย่างไรก็ตาม, สำหรับประสบการณ์ที่มองเห็นได้มากกว่าหรือแอปพลิเคชันขนาดใหญ่ คุณอาจสำรวจ IDE ที่มีตัวดีบักในตัว, เช่น Visual Studio Code พร้อมส่วนขยายของ Haskell หรือปลั๊กอิน Haskell ของ IntelliJ

ทางเลือกอื่นสำหรับตัวดีบัก ได้แก่การใช้คำสั่งพิมพ์, ที่รู้จักกันในชื่อ "printf debugging", หรือใช้ประโยชน์จากระบบการพิมพ์ที่แข็งแกร่งของ Haskell เพื่อทำให้สถานะที่ไม่ถูกต้องไม่สามารถแทนที่ได้ อย่างไรก็ตาม, บางครั้งการเดินขั้นโค้ดไม่มีอะไรทดแทนได้

สำหรับรายละเอียดการดำเนินการ, ตัวดีบักของ Haskell ทำงานกับระบบการรันไทม์ มันสามารถจัดการกับจุดหยุด, ขั้นตอนการดำเนินการ, และอนุญาตให้ตรวจสอบตัวแปรได้ อย่างไรก็ตาม, เนื่องจาก Haskell มีการประเมินแบบเฉื่อย สิ่งต่างๆ อาจดูไม่ตรงไปตรงมานัก การดีบักโปรแกรม Haskell มักหมายถึงการระมัดระวังโดยเมื่อและวิธีการที่นิพจน์ถูกประเมิน

## ดูเพิ่มเติม:
- [คู่มือผู้ใช้ GHC - Debugger](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [ปลั๊กอิน Haskell ของ IntelliJ](https://plugins.jetbrains.com/plugin/8258-intellij-haskell)

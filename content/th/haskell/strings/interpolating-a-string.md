---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:53.463336-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E15\u0E31\u0E27\u0E41\u0E1B\
  \u0E23\u0E25\u0E07\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 (String interpolation)\
  \ \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E1D\u0E31\u0E07\
  \u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\u0E25\u0E07\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\u2026"
lastmod: '2024-03-17T21:57:56.254404-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E15\u0E31\u0E27\u0E41\u0E1B\
  \u0E23\u0E25\u0E07\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 (String interpolation)\
  \ \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E1D\u0E31\u0E07\
  \u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\u0E25\u0E07\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E04\u0E48\u0E32\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การแทรกตัวแปรลงในสตริง (String interpolation) ช่วยให้คุณฝังตัวแปรลงในสตริงได้โดยตรง มันทำได้เพื่อความสะดวกและทำให้อ่านง่ายขึ้น—ไม่จำเป็นต้องใช้เครื่องหมายบวกหรือเรียกฟังก์ชันเพื่อสร้างข้อความของคุณ

## วิธีทำ:

ใน Haskell, การแทรกตัวแปรลงในสตริงไม่ได้เป็นส่วนหนึ่งมาแต่ต้น แต่ด้วยแพ็คเกจ `interpolate` คุณสามารถทำได้เกือบเหมือนกัน ก่อนอื่น, ตรวจสอบว่าคุณมีแพ็คเกจ:

```bash
cabal update
cabal install interpolate
```

ตอนนี้, เขียนฮาสเกลบางอย่าง:

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Data.String.Interpolate (i)

main :: IO ()
main = do
    let name = "world"
    let greeting = [i|Hello, #{name}!|]
    putStrLn greeting
```

เรียกใช้:

```
Hello, world!
```

## ภาพรวมลึกซึ้ง

ในอดีต, Haskell ไม่มีการแทรกตัวแปรลงในสตริงมาแต่อย่างใด มันเป็นคุณสมบัติที่พบในภาษาสคริปต์มากกว่า การแทรกตัวแปรทำได้ง่ายขึ้นใน Haskell ด้วยการพัฒนาของ quasiquoters ซึ่งช่วยให้คุณกำหนดไวยากรณ์ของตัวเอง—เช่น `i` ของเราสำหรับการแทรกสตริง

มีทางเลือกอื่นหรือไม่? แน่นอน, ใช้ `printf` จาก `Text.Printf`, หรือต่อสตริงและตัวแปรด้วย `++` แต่วิธีเหล่านี้ขาดความเรียบง่ายและสะดวกของการแทรกตัวแปร

ในแง่ของการนำไปใช้, `interpolate` แปลงสตริงที่ได้รับการแทรกตัวแปรของคุณเป็นสตริงฮาสเกลปกติที่เวลาคอมไพล์โดยใช้ Template Haskell, ดังนั้นจึงไม่มีการกระทบต่อประสิทธิภาพเมื่อเรียกใช้โค้ดของคุณ มันเป็นวิธีที่ชาญฉลาดและสะอาด, เหมือนฮาสเกลเอง

## ดูเพิ่มเติม

- [Hackage - แพ็คเกจ interpolate](https://hackage.haskell.org/package/interpolate)
- [Hackage - โมดูล Text.Printf](https://hackage.haskell.org/package/base/docs/Text-Printf.html)
- [Haskell Wiki - Quasiquotation](https://wiki.haskell.org/Quasiquotation)
- สำหรับการจัดแม่แบบที่หลากหลาย, ตรวจสอบ [Hackage - แม่แบบ Mustache](https://hackage.haskell.org/package/mustache)

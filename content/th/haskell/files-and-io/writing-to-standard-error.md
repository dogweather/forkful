---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:09.444284-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Haskell \u0E01\
  \u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\
  \u0E44\u0E1B\u0E22\u0E31\u0E07 stderr \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\
  \u0E33\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E14\
  \u0E32\u0E22\u0E14\u0E49\u0E27\u0E22\u0E42\u0E21\u0E14\u0E39\u0E25 `System.IO` \u0E02\
  \u0E2D\u0E07\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E1E\u0E37\u0E49\u0E19\u0E10\
  \u0E32\u0E19 \u0E15\u0E48\u0E2D\u0E44\u0E1B\u0E19\u0E35\u0E49\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\
  \u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E2A\u0E14\u0E07."
lastmod: '2024-03-17T21:57:56.284376-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Haskell \u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E44\u0E1B\u0E22\u0E31\u0E07 stderr \u0E2A\
  \u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E07\u0E48\u0E32\u0E22\u0E14\u0E32\u0E22\u0E14\u0E49\u0E27\u0E22\u0E42\u0E21\
  \u0E14\u0E39\u0E25 `System.IO` \u0E02\u0E2D\u0E07\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\
  \u0E35\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19 \u0E15\u0E48\u0E2D\u0E44\u0E1B\u0E19\
  \u0E35\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E2A\
  \u0E14\u0E07."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19"
weight: 25
---

## วิธีการ:
ใน Haskell การเขียนข้อความไปยัง stderr สามารถทำได้อย่างง่ายดายด้วยโมดูล `System.IO` ของไลบรารีพื้นฐาน ต่อไปนี้เป็นตัวอย่างพื้นฐานเพื่อแสดง:

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "This is an error message."
```

ผลลัพธ์ของโปรแกรมนี้ที่ stderr จะเป็น:

```
This is an error message.
```

หากคุณกำลังทำงานในแอปพลิเคชันที่ซับซ้อนมากขึ้น หรือหากคุณต้องการควบคุมการบันทึกข้อมูล (รวมถึงข้อผิดพลาด) ได้ดียิ่งขึ้น คุณอาจเลือกใช้ไลบรารีของบุคคลที่สาม ตัวเลือกยอดนิยมคือ `monad-logger` ซึ่งรวมเข้ากับสไตล์การเขียนโปรแกรม Haskell แบบ `mtl` นี่คือสนิปเพ็ตเล็กๆ โดยใช้ `monad-logger`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "This is an error message using monad-logger."
```

เมื่อทำงาน การใช้ `monad-logger` จะได้ผลลัพธ์ข้อความแสดงข้อผิดพลาดเช่นกัน แต่จะมาพร้อมกับบริบทมากขึ้น เช่น ตราประทับเวลาหรือระดับของเข้าข้อความล็อก ซึ่งขึ้นอยู่กับการตั้งค่า:

```
[Error] This is an error message using monad-logger.
```

ทั้งสองวิธีให้บริการเพื่อการเขียนข้อความไปที่ stderr โดยการเลือกวิธีการขึ้นอยู่กับความซับซ้อนและความต้องการของแอปพลิเคชันของคุณ

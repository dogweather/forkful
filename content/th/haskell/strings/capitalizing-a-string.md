---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:39.970420-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19\u0E20\u0E32\
  \u0E29\u0E32 Haskell, \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\
  \u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\
  \u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E44\u0E14\u0E49\
  \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E21\
  \u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\
  \u0E2D\u0E07\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E02\u0E2D\
  \u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21."
lastmod: '2024-03-17T21:57:56.251262-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19\u0E20\u0E32\u0E29\u0E32 Haskell, \u0E04\u0E38\u0E13\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\u0E2B\
  \u0E0D\u0E48\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\
  \u0E23\u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E42\u0E14\u0E22\
  \u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\
  \u0E32\u0E23\u0E35\u0E02\u0E2D\u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\u0E17\u0E35\u0E48\
  \u0E2A\u0E32\u0E21."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E15\u0E31\u0E27\u0E2D\u0E31\
  \u0E01\u0E29\u0E23\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 2
---

## วิธีการ:
ในภาษา Haskell, คุณสามารถทำให้สตริงเป็นตัวพิมพ์ใหญ่ได้โดยใช้ไลบรารีมาตรฐานโดยไม่ต้องใช้ไลบรารีของบุคคลที่สาม

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- ตัวอย่างการใช้งาน:
main = putStrLn $ capitalize "hello world"
```

ผลลัพธ์:
```
Hello world
```

สำหรับสถานการณ์ที่ซับซ้อนมากขึ้นหรือความง่ายในการใช้งาน, คุณอาจต้องการใช้ไลบรารีของบุคคลที่สาม เช่น `text`, ซึ่งเป็นที่นิยมสำหรับการจัดการสตริงอย่างมีประสิทธิภาพในภาษา Haskell

ก่อนอื่น, คุณต้องเพิ่ม `text` เข้าไปใน dependencies ของโปรเจกต์ของคุณ จากนั้น, คุณสามารถใช้ฟังก์ชั่นของมันในการทำให้สตริงเป็นตัวพิมพ์ใหญ่ดังต่อไปนี้:

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- ตัวอย่างการใช้งานกับไลบรารี text:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

ผลลัพธ์:
```
Hello world
```

ทั้งสองตัวอย่างนี้แสดงวิธีการที่ง่ายและมีประสิทธิภาพในการทำให้สตริงเป็นตัวพิมพ์ใหญ่ในภาษา Haskell, โดยมีหรือไม่มีไลบรารีของบุคคลที่สาม

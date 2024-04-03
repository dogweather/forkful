---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:05.733773-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E15\u0E31\u0E49\u0E07\
  \u0E41\u0E15\u0E48\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19, Haskell \u0E21\
  \u0E35\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\u0E37\u0E2D\u0E1E\u0E37\u0E49\
  \u0E19\u0E10\u0E32\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\
  \u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48, \u0E41\u0E15\u0E48\u0E01\
  \u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\
  \u0E35 \u0E40\u0E0A\u0E48\u0E19 `time` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E1F\
  \u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E2B\u0E25\u0E31\u0E01\u0E41\u0E25\u0E30\
  \ `date-parse` \u0E2B\u0E23\u0E37\u0E2D `time-\u2026"
lastmod: '2024-03-17T21:57:56.277429-06:00'
model: gpt-4-0125-preview
summary: "\u0E15\u0E31\u0E49\u0E07\u0E41\u0E15\u0E48\u0E40\u0E23\u0E34\u0E48\u0E21\
  \u0E15\u0E49\u0E19, Haskell \u0E21\u0E35\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\
  \u0E21\u0E37\u0E2D\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48, \u0E41\u0E15\u0E48\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\
  \u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35 \u0E40\u0E0A\u0E48\u0E19 `time` \u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E2B\
  \u0E25\u0E31\u0E01\u0E41\u0E25\u0E30 `date-parse` \u0E2B\u0E23\u0E37\u0E2D `time-parse`\
  \ \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\
  \u0E17\u0E35\u0E48\u0E22\u0E37\u0E14\u0E2B\u0E22\u0E38\u0E48\u0E19\u0E21\u0E32\u0E01\
  \u0E02\u0E36\u0E49\u0E19 \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E25\u0E14\u0E04\u0E27\
  \u0E32\u0E21\u0E22\u0E38\u0E48\u0E07\u0E22\u0E32\u0E01\u0E02\u0E2D\u0E07\u0E07\u0E32\
  \u0E19\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E21\u0E32\u0E01\n\n\u0E01\
  \u0E48\u0E2D\u0E19\u0E2D\u0E37\u0E48\u0E19, \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\
  \u0E1A\u0E27\u0E48\u0E32\u0E04\u0E38\u0E13\u0E21\u0E35\u0E44\u0E25\u0E1A\u0E23\u0E32\
  \u0E23\u0E35 `time` \u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E43\u0E0A\u0E49\u0E07\u0E32\
  \u0E19; \u0E21\u0E31\u0E19\u0E21\u0E31\u0E01\u0E08\u0E30\u0E16\u0E39\u0E01\u0E23\
  \u0E27\u0E21\u0E2D\u0E22\u0E39\u0E48\u0E01\u0E31\u0E1A GHC, \u0E41\u0E15\u0E48\u0E16\
  \u0E49\u0E32\u0E04\u0E38\u0E13\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E23\u0E30\
  \u0E1A\u0E38\u0E40\u0E1B\u0E47\u0E19\u0E01\u0E32\u0E23\u0E1E\u0E36\u0E48\u0E07\u0E1E\
  \u0E32, \u0E40\u0E1E\u0E34\u0E48\u0E21 `time` \u0E25\u0E07\u0E43\u0E19\u0E44\u0E1F\
  \u0E25\u0E4C cabal \u0E02\u0E2D\u0E07\u0E42\u0E1B\u0E23\u0E40\u0E08\u0E04\u0E2B\u0E23\
  \u0E37\u0E2D\u0E43\u0E0A\u0E49 `cabal install time` \u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E01\u0E32\u0E23\u0E15\u0E34\u0E14\u0E15\u0E31\u0E49\u0E07\u0E14\u0E49\u0E27\u0E22\
  \u0E15\u0E31\u0E27\u0E40\u0E2D\u0E07."
title: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 30
---

## วิธีการ:
ตั้งแต่เริ่มต้น, Haskell มีเครื่องมือพื้นฐานสำหรับการแปลงวันที่, แต่การใช้งานไลบรารี เช่น `time` สำหรับฟังก์ชันหลักและ `date-parse` หรือ `time-parse` สำหรับการแปลงที่ยืดหยุ่นมากขึ้น สามารถลดความยุ่งยากของงานได้อย่างมาก

ก่อนอื่น, ตรวจสอบว่าคุณมีไลบรารี `time` พร้อมใช้งาน; มันมักจะถูกรวมอยู่กับ GHC, แต่ถ้าคุณต้องการระบุเป็นการพึ่งพา, เพิ่ม `time` ลงในไฟล์ cabal ของโปรเจคหรือใช้ `cabal install time` เพื่อการติดตั้งด้วยตัวเอง.

```haskell
import Data.Time.Format
import Data.Time.Clock
import System.Locale (defaultTimeLocale)

-- การใช้งานไลบรารี time เพื่อแปลงวันที่ในรูปแบบมาตรฐาน
parseBasicDate :: String -> Maybe UTCTime
parseBasicDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" 
```

ตัวอย่างการใช้งานและผลลัพธ์:

```haskell
main :: IO ()
main = print $ parseBasicDate "2023-04-01"

-- ผลลัพธ์: Just 2023-03-31 22:00:00 UTC
```

สำหรับสถานการณ์ที่ซับซ้อนขึ้น, ซึ่งคุณต้องการจัดการกับรูปแบบหรือตำแหน่งทางภูมิศาสตร์หลาย ๆ แบบ, ไลบรารีของบุคคลที่สาม เช่น `date-parse` อาจสะดวกมากขึ้น:

ถ้าคุณได้เพิ่ม `date-parse` เป็นการพึ่งพาและติดตั้งมัน, นี่คือวิธีที่คุณอาจใช้งานมัน:

```haskell
import Data.Time.Calendar
import Text.Date.Parse (parseDate)

-- การแปลงสตริงวันที่ด้วยไลบรารี date-parse รองรับรูปแบบหลายแบบ
parseFlexibleDate :: String -> Maybe Day
parseFlexibleDate = parseDate
```

ตัวอย่างการใช้งานกับ `date-parse`:

```haskell
main :: IO ()
main = print $ parseFlexibleDate "April 1, 2023"

-- ผลลัพธ์: Just 2023-04-01
```

แต่ละตัวอย่างแสดงวิธีการพื้นฐานในการนำสตริงมาแปลงเป็นวันที่ที่สามารถใช้งานได้ใน Haskell. การเลือกระหว่างการใช้ฟังก์ชันที่มีอยู่ในไลบรารี `time` และการเลือกใช้ไลบรารีของบุคคลที่สาม เช่น `date-parse` ขึ้นอยู่กับความต้องการเฉพาะของแอปพลิเคชันของคุณ, เช่น ช่วงของรูปแบบข้อมูลนำเข้าที่คุณต้องการจัดการ.

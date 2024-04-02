---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:08.075276-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\
  \u0E43\u0E19\u0E20\u0E32\u0E29\u0E32 Haskell \u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\
  \u0E14\u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E44\u0E14\u0E49\u0E23\u0E31\u0E1A\u0E40\
  \u0E27\u0E25\u0E32\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\u0E02\u0E2D\u0E07\
  \u0E23\u0E30\u0E1A\u0E1A\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\
  \u0E21\u0E31\u0E19\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\
  \u0E49\u2026"
lastmod: '2024-03-17T21:57:56.278525-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\
  \u0E43\u0E19\u0E20\u0E32\u0E29\u0E32 Haskell \u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\
  \u0E14\u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E44\u0E14\u0E49\u0E23\u0E31\u0E1A\u0E40\
  \u0E27\u0E25\u0E32\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\u0E02\u0E2D\u0E07\
  \u0E23\u0E30\u0E1A\u0E1A\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\
  \u0E21\u0E31\u0E19\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\
  \u0E49\u2026"
title: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\
  \u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19"
weight: 29
---

## อะไรและทำไม?
การดึงข้อมูลวันที่ปัจจุบันในภาษา Haskell ประกอบด้วยการได้รับเวลาปัจจุบันของระบบและการแปลงมันเป็นรูปแบบวันที่ที่อ่านได้ โปรแกรมเมอร์ทำเช่นนี้เพื่อดำเนินการต่างๆ ตามวันที่ เช่น การบันทึกล็อก การกำหนดการทำงาน หรือการประทับเวลาในแอปพลิเคชัน

## วิธีการ:
ไลบรารีมาตรฐานของ Haskell, `base`, มีโมดูล `Data.Time` ซึ่งเสนอฟังก์ชันการทำงานกับวันที่และเวลา นี่คือวิธีใช้งานเพื่อรับข้อมูลวันที่ปัจจุบัน:

```haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

ตัวอย่างผลลัพธ์:
```
2023-04-12
```

สำหรับความยืดหยุ่นมากขึ้น เช่น การจัดรูปแบบวันที่หรือการทำงานกับเขตเวลาที่แตกต่างกัน ไลบรารี `time` มีค่าอย่างมาก นี่คือวิธีที่คุณอาจจัดรูปแบบวันที่ปัจจุบัน:

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" zoneNow
```

ซึ่งจะพิมพ์วันที่ปัจจุบันในรูปแบบ `YYYY-MM-DD`, ปรับให้เข้ากับเขตเวลาท้องถิ่น

นอกจากนี้ สำหรับการสนับสนุนไลบรารีบุคคลที่สาม `time` ได้รับการแนะนำอย่างสูงและมักจะใช้ในชุมชน Haskell เนื่องจากความสามารถในการจัดการวันที่และเวลาที่กว้างขวาง เอ็กซ์แซมเปิ้ลข้างต้นใช้ไลบรารีนี้

หากคุณต้องการการจัดการวันที่ที่ครอบคลุมมากขึ้น รวมถึงการแปลงข้อมูลจากสตริงหรือการดำเนินการทางคณิตศาสตร์กับวันที่และเวลา การสำรวจฟังก์ชันเพิ่มเติมภายใน `Data.Time` จะเป็นประโยชน์

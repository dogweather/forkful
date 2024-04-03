---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:51.031446-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Haskell \u0E0B\u0E36\u0E48\
  \u0E07\u0E40\u0E1B\u0E47\u0E19\u0E17\u0E35\u0E48\u0E23\u0E39\u0E49\u0E08\u0E31\u0E01\
  \u0E43\u0E19\u0E14\u0E49\u0E32\u0E19\u0E04\u0E27\u0E32\u0E21\u0E1A\u0E23\u0E34\u0E2A\
  \u0E38\u0E17\u0E18\u0E34\u0E4C, \u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E43\
  \u0E2B\u0E49\u0E04\u0E38\u0E13\u0E1E\u0E39\u0E14\u0E20\u0E32\u0E29\u0E32\u0E40\u0E01\
  \u0E35\u0E48\u0E22\u0E27\u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E14\
  \u0E49\u0E27\u0E22\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E17\u0E35\u0E48\u0E16\
  \u0E39\u0E01\u0E15\u0E49\u0E2D\u0E07 \u0E25\u0E2D\u0E07\u0E43\u0E0A\u0E49 `Data.Time`."
lastmod: '2024-03-17T21:57:56.280610-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E17\u0E35\u0E48\
  \u0E23\u0E39\u0E49\u0E08\u0E31\u0E01\u0E43\u0E19\u0E14\u0E49\u0E32\u0E19\u0E04\u0E27\
  \u0E32\u0E21\u0E1A\u0E23\u0E34\u0E2A\u0E38\u0E17\u0E18\u0E34\u0E4C, \u0E15\u0E49\
  \u0E2D\u0E07\u0E01\u0E32\u0E23\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E1E\u0E39\u0E14\
  \u0E20\u0E32\u0E29\u0E32\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E01\u0E31\u0E1A\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E14\u0E49\u0E27\u0E22\u0E44\u0E25\u0E1A\u0E23\u0E32\
  \u0E23\u0E35\u0E17\u0E35\u0E48\u0E16\u0E39\u0E01\u0E15\u0E49\u0E2D\u0E07 \u0E25\u0E2D\
  \u0E07\u0E43\u0E0A\u0E49 `Data.Time`."
title: "\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\u0E22\u0E1A\u0E2A\u0E2D\
  \u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48"
weight: 27
---

## วิธีการ:
Haskell ซึ่งเป็นที่รู้จักในด้านความบริสุทธิ์, ต้องการให้คุณพูดภาษาเกี่ยวกับวันที่ด้วยไลบรารีที่ถูกต้อง ลองใช้ `Data.Time`

```haskell
import Data.Time

-- กำหนดวันที่สองวัน
date1 :: UTCTime
date1 = UTCTime (fromGregorian 2023 4 1) (secondsToDiffTime 0)

date2 :: UTCTime
date2 = UTCTime (fromGregorian 2024 4 2) (secondsToDiffTime 3600)

-- เปรียบเทียบวันที่
compareDates :: UTCTime -> UTCTime -> Ordering
compareDates = compare

main :: IO ()
main = do
    print $ date1 `compareDates` date2 -- ผลลัพธ์จะเป็น LT
    print $ date2 `compareDates` date1 -- ผลลัพธ์จะเป็น GT
    print $ date1 `compareDates` date1 -- ผลลัพธ์จะเป็น EQ
```

ตรงไปตรงมาเลยใช่ไหม? `LT` สำหรับน้อยกว่า, `GT` สำหรับมากกว่า, และ `EQ` สำหรับเท่ากัน

## ภาพรวม
ในอดีต, การจัดการเวลาของ Haskell ไม่ได้เรียบร้อยเหมือนปัจจุบัน เราต้องขอบคุณการพัฒนาของไลบรารี `Data.Time` ตลอดหลายปีที่ผ่านมา มันให้เรา `UTCTime`, ซึ่งเป็นจุดเวลาที่ไม่มีความกำกวม

มีทางเลือกอื่นหรือไม่? แน่นอน คุณอาจพบว่า `Data.Time.Calendar` และ `Data.Time.Clock` มีประโยชน์สำหรับสถานการณ์เฉพาะ นอกจากนี้ยังมีไลบรารี `time` แบบเก่าสำหรับผู้ที่รู้สึกโหยหาหรือติดกับโค้ดที่เป็นมรดก

ตอนนี้, รายละเอียดที่สำคัญ: การเปรียบเทียบวันที่ใน Haskell ขึ้นอยู่กับ `UTCTime` ซึ่งคู่กันระหว่างวัน (`Day`) และเวลา (`DiffTime` หรือ `NominalDiffTime`) เป็นฟังก์ชัน `compare` ที่ทำหน้าที่หนัก, สมาชิกที่น่าสนใจของคลาส `Ord`, ช่วยให้เราใช้ `>, <, ==` และอื่น ๆ อีกมากมาย แค่จำไว้ว่า Haskell รักความปลอดภัยของชนิด ให้แน่ใจว่าคุณเปรียบเทียบแอปเปิ้ลกับแอปเปิ้ลเสมอ, หรือในกรณีของเรา, `UTCTime` กับ `UTCTime`

## ดูเพิ่มเติม
ขุดลึกขึ้นหรือหาความช่วยเหลือด้วยเหล่านี้:
- ไลบรารี [`Data.Time` ที่ Hackage](https://hackage.haskell.org/package/time-1.11/docs/Data-Time.html)
- [Learn You a Haskell for Great Good! – สำหรับการแนะนำอย่างเบาสบาย](http://learnyouahaskell.com/)
- [Stack Overflow สำหรับการแก้ไขปัญหาในโลกแห่งความจริง](https://stackoverflow.com/questions/tagged/haskell+time)

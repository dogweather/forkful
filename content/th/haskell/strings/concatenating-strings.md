---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:49.428566-06:00
description: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E40\u0E0A\u0E37\u0E2D\u0E01\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E23\u0E27\u0E21\u0E40\u0E0A\u0E37\u0E2D\u0E01\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E40\u0E02\u0E49\u0E32\u0E14\u0E49\u0E27\u0E22\u0E01\u0E31\u0E19\
  \u0E15\u0E48\u0E2D\u0E17\u0E49\u0E32\u0E22\u0E01\u0E31\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.260241-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E40\u0E0A\u0E37\u0E2D\u0E01\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E23\u0E27\u0E21\u0E40\u0E0A\u0E37\u0E2D\u0E01\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E40\u0E02\u0E49\u0E32\u0E14\u0E49\u0E27\u0E22\u0E01\u0E31\u0E19\
  \u0E15\u0E48\u0E2D\u0E17\u0E49\u0E32\u0E22\u0E01\u0E31\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 3
---

## อะไรและทำไม?
การต่อเชือกข้อความหมายถึงการรวมเชือกข้อความเข้าด้วยกันต่อท้ายกัน โปรแกรมเมอร์ทำอย่างนี้เมื่อพวกเขาต้องการเย็บชิ้นส่วนของข้อความเข้าด้วยกันเพื่อสร้างข้อความใหม่ เช่น การสร้างข้อความหรือสร้างเส้นทางไฟล์

## วิธีการ:
Haskell ทำให้การต่อเชือกข้อความเป็นเรื่องง่ายด้วยตัวดำเนินการ `(++)`:

```Haskell
main :: IO ()
main = do
  let hello = "Hello"
  let world = "World!"
  
  -- ใช้ตัวดำเนินการ (++)
  putStrLn $ hello ++ " " ++ world
  
  -- ตัวอย่างผลลัพธ์: "Hello World!"
```

แต่ทำไมต้องหยุดที่นี่? คุณยังมี `concat` และ `intercalate` จาก `Data.List` สําหรับเมื่อมีรายการเข้ามาเกี่ยวข้อง:

```Haskell
import Data.List (intercalate, concat)

main :: IO ()
main = do
  let wordsList = ["Haskell", "is", "cool"]
  
  -- การต่อรายการข้อความ
  putStrLn $ concat wordsList
  -- ตัวอย่างผลลัพธ์: "Haskelliscool"

  -- การแทรกข้อความด้วยตัวคั่น
  putStrLn $ intercalate " " wordsList
  -- ตัวอย่างผลลัพธ์: "Haskell is cool"
```

## ลงลึก
ย้อนกลับไปในอดีต, ตัวดำเนินการ `++` ของ Haskell ได้รับแรงบันดาลใจจากการดำเนินการที่คล้ายกันในภาษาอื่นๆ เช่น ML มันเป็นคลาสสิก แต่ไม่เสมอไปที่จะมีประสิทธิภาพสูงสุด โดยเฉพาะสําหรับข้อความขนาดใหญ่หรืองานการต่อข้อความจำนวนมาก การใช้ `++` ทุกครั้งจะสร้างรายการใหม่ ซึ่งหมายความว่าหากคุณกำลังทำงานกับข้อมูลขนาดใหญ่ คุณอาจต้องการวิธีที่มีประสิทธิภาพมากขึ้น

มีทางเลือกอื่นหรือไม่? แน่นอน ประเภท `Builder` จาก `Data.Text.Lazy.Builder` สามารถถูกปรับให้เหมาะสมกับการจัดการข้อความขนาดใหญ่ได้ดียิ่งขึ้น มันสร้างข้อความได้มีประสิทธิภาพมากขึ้นโดยการทำงานเป็นชิ้น ลดความจำเป็นในการคัดลอกเสี้ยวจนตลอดเวลา

ตัวอย่างเช่น การทำงานกับ `Builder`:

```Haskell
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import Data.Text.Lazy.IO as T

main :: IO ()
main = do
  let builder1 = fromString "Haskell"
  let builder2 = fromString " "
  let builder3 = fromString "is"
  let builder4 = fromString " "
  let builder5 = fromString "neat!"

  let result = mconcat [builder1, builder2, builder3, builder4, builder5]
  -- ใช้ mconcat เพื่อผสาน Builders เข้าด้วยกัน

  T.putStrLn $ toLazyText result
  -- ตัวอย่างผลลัพธ์: "Haskell is neat!"
```

ทำไมต้องเลือกใช้ `Builder` หรือ `concat`? พวกเขาจัดการกับข้อมูลขนาดใหญ่โดยไม่โอ้โหต่อประสิทธิภาพ ช่วยให้คุณสามารถรวมข้อความได้โดยไม่มีปัญหาในเรื่องประสิทธิภาพ

## ดูเพิ่มเติม
- Haskell Wiki เกี่ยวกับ [Performance/Strings](https://wiki.haskell.org/Performance/Strings) เพื่อศึกษาเพิ่มเติมเกี่ยวกับการพิจารณาประสิทธิภาพ
- เอกสารการใช้งานของ [package Data.Text](https://hackage.haskell.org/package/text) สำหรับการทำงานกับข้อความ Unicode ใน Haskell
- เว็บไซต์ [ภาษา Haskell](https://www.haskell.org/) เพื่อติดตามอัปเดตทุกอย่างเกี่ยวกับ Haskell

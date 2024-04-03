---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:33.043077-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E27\u0E31\u0E15\u0E16\u0E38\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E21\u0E32\u0E41\u0E1B\u0E25\u0E07\u0E43\u0E2B\u0E49\
  \u0E40\u0E1B\u0E47\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\
  \u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\u0E49\u2026"
lastmod: '2024-03-17T21:57:56.279680-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E27\u0E31\u0E15\u0E16\u0E38\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E21\u0E32\u0E41\u0E1B\u0E25\u0E07\u0E43\u0E2B\u0E49\
  \u0E40\u0E1B\u0E47\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\
  \u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\u0E49 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E2A\u0E14\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E43\u0E2B\u0E49\u0E01\u0E31\u0E1A\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E2B\
  \u0E23\u0E37\u0E2D\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E23\u0E39\u0E1B\
  \u0E41\u0E1A\u0E1A\u0E43\u0E2B\u0E49\u0E40\u0E2B\u0E21\u0E32\u0E30\u0E2A\u0E21\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E40\u0E01\u0E47\
  \u0E1A\u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25."
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 28
---

## อะไร & ทำไม?
การแปลงวันที่เป็นสตริงหมายถึงการนำวัตถุวันที่มาแปลงให้เป็นข้อความที่อ่านได้ โปรแกรมเมอร์ทำเช่นนี้เพื่อแสดงวันที่ให้กับผู้ใช้หรือเพื่อจัดรูปแบบให้เหมาะสมสำหรับการจัดเก็บหรือการส่งข้อมูล

## วิธีการ:
ใน Haskell, คุณใช้ฟังก์ชัน `formatTime` จากโมดูล `Data.Time.Format` สำหรับงานนี้ มาดูโค้ดกันเลย:

```haskell
import Data.Time
import Data.Time.Format (formatTime, defaultTimeLocale)

main :: IO ()
main = do
    -- ดึงเวลาปัจจุบัน
    currentTime <- getCurrentTime
    let currentZone = utc
        -- แปลงเวลา UTC เป็นวัตถุเวลาท้องถิ่น
        localTime = utcToLocalTime currentZone currentTime
        -- จัดรูปแบบวันที่เป็น "YYYY-MM-DD"
        dateString = formatTime defaultTimeLocale "%F" localTime
    putStrLn dateString
```

และนี่คือสิ่งที่คุณอาจเห็นเป็นผลลัพธ์ ขึ้นอยู่กับวันที่ปัจจุบัน:

```
2023-04-01
```

## ลงลึก
ย้อนกลับไปในยุคแรกๆ ของการเขียนโปรแกรม การแปลงวันที่เป็นสตริงเป็นเรื่องของการใช้งานจริง ใน Haskell, เรามีห้องสมุด `Data.Time` เพื่อจัดการกับวันและเวลา ซึ่งได้รับแรงบันดาลใจจากฟังก์ชันและการปรับปรุงเหนือห้องสมุดเก่าๆ เช่น `old-time`

มีทางเลือกอื่นๆ แก่ `formatTime`, เช่นการใช้ `show` เพื่อแปลงวันที่เป็นสตริงโดยตรง แต่นี้จะไม่ให้ตัวเลือกการจัดรูปแบบที่เฉพาะเจาะจง ฟังก์ชัน `formatTime` เป็นฟังก์ชันที่มีความหลากหลาย สนับสนุนรูปแบบการจัดรูปแบบที่หลากหลายตามแบบฟังก์ชัน `strftime` ของภาษา C มันยืดหยุ่นและตระหนักถึงสภาพแวดล้อมท้องถิ่น โดยใช้ `defaultTimeLocale` หรือสภาพแวดล้อมท้องถิ่นอื่นๆ เพื่อจัดรูปแบบวันที่ตามธรรมเนียมวัฒนธรรม

เกี่ยวกับการนำไปใช้งาน, ฟังก์ชันใน `Data.Time.Format` เป็นฟังก์ชันแบบบริสุทธิ์ หมายความว่าพวกมันไม่อาศัยหรือก่อให้เกิดผลลัพธ์แตกต่างจากปกติ สิ่งนี้สอดคล้องกับจิตวิญญาณการเขียนโปรแกรมแบบฟังก์ชันของ Haskell ซึ่งมุ่งเน้นให้ฟังก์ชันนั้นคาดเดาได้และผลลัพธ์ของพวกมันถูกกำหนดเพียงจากอินพุตของพวกมันเท่านั้น

## ดูเพิ่มเติม
สำหรับงานที่กว้างขึ้นเกี่ยวกับวันที่และเวลาใน Haskell, สำรวจสิ่งต่อไปนี้:

- คู่มือโมดูล `Data.Time`: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- รายละเอียดเกี่ยวกับสตริงรูปแบบ `strftime`, ซึ่ง `formatTime` เลียนแบบ: [http://man7.org/linux/man-pages/man3/strftime.3.html](http://man7.org/linux/man-pages/man3/strftime.3.html)
- วิธีการของ Haskell ในการจัดการกับ IO และความบริสุทธิ์: [https://www.haskell.org/tutorial/io.html](https://www.haskell.org/tutorial/io.html)

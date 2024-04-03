---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:43.655673-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: Haskell \u0E1C\u0E48\u0E32\u0E19\
  \u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E10\u0E32\u0E19\u0E02\u0E2D\u0E07\u0E21\
  \u0E31\u0E19 \u0E40\u0E2A\u0E19\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E15\u0E23\u0E27\u0E08\
  \u0E2A\u0E2D\u0E1A\u0E01\u0E32\u0E23\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E02\u0E2D\
  \u0E07\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32 \u0E42\u0E14\u0E22\
  \u0E2B\u0E25\u0E31\u0E01\u0E43\u0E0A\u0E49\u0E42\u0E21\u0E14\u0E39\u0E25 `System.Directory`\
  \ \u0E21\u0E32\u0E14\u0E39\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E1E\
  \u0E37\u0E49\u0E19\u0E10\u0E32\u0E19."
lastmod: '2024-03-17T21:57:56.282567-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u0E1C\u0E48\u0E32\u0E19\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\
  \u0E10\u0E32\u0E19\u0E02\u0E2D\u0E07\u0E21\u0E31\u0E19 \u0E40\u0E2A\u0E19\u0E2D\u0E27\
  \u0E34\u0E18\u0E35\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E01\u0E32\u0E23\u0E21\
  \u0E35\u0E2D\u0E22\u0E39\u0E48\u0E02\u0E2D\u0E07\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\
  \u0E2D\u0E23\u0E35\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\
  \u0E23\u0E07\u0E21\u0E32 \u0E42\u0E14\u0E22\u0E2B\u0E25\u0E31\u0E01\u0E43\u0E0A\u0E49\
  \u0E42\u0E21\u0E14\u0E39\u0E25 `System.Directory` \u0E21\u0E32\u0E14\u0E39\u0E15\
  \u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\
  ."
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
weight: 20
---

## วิธีทำ:
Haskell ผ่านไลบรารีฐานของมัน เสนอวิธีตรวจสอบการมีอยู่ของไดเรกทอรีอย่างตรงไปตรงมา โดยหลักใช้โมดูล `System.Directory` มาดูตัวอย่างพื้นฐาน:

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "/path/to/your/directory"
  exists <- doesDirectoryExist dirPath
  putStrLn $ "Does the directory exist? " ++ show exists
```

ตัวอย่างผลลัพธ์ ขึ้นอยู่กับว่าไดเรกทอรีมีอยู่หรือไม่:

```
Does the directory exist? True
```
หรือ:
```
Does the directory exist? False
```

สำหรับสถานการณ์ที่ซับซ้อนขึ้นหรือการทำงานพิเศษเพิ่มเติม คุณอาจพิจารณาไลบรารีภายนอกที่นิยมเช่น `filepath` สำหรับการจัดการและแปลงเส้นทางไฟล์ในลักษณะที่มีการแปลกแยกมากขึ้น อย่างไรก็ตาม สำหรับวัตถุประสงค์ในการตรวจสอบเพียงว่ามีไดเรกทอรีอยู่หรือไม่ ไลบรารีฐาน `System.Directory` ก็เพียงพอและมีประสิทธิภาพ

จำไว้ว่า การทำงานกับระบบไฟล์อาจแตกต่างกันไปตามแพลตฟอร์ม และแนวทางของ Haskell มุ่งเน้นที่จะทำให้บางส่วนของความแตกต่างเหล่านี้ไม่เด่นชัด ควรทดสอบการดำเนินการกับไฟล์ของคุณบนระบบเป้าหมายเพื่อให้แน่ใจว่ามีพฤติกรรมตามที่คาดหวัง

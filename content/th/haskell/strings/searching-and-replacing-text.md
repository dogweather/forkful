---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:43.463844-06:00
description: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\
  \u0E17\u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E1E\u0E1A\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E41\u0E25\u0E30\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\
  \u0E07\u0E21\u0E31\u0E19\u0E2D\u0E2D\u0E01\u0E44\u0E1B\u0E44\u0E14\u0E49 \u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E21\
  \u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2D\u0E31\u0E1B\u0E40\u0E14\u0E15\u0E42\
  \u0E04\u0E49\u0E14, \u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07, \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\
  \u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\
  \u0E14\u0E40\u0E23\u0E47\u0E27"
lastmod: '2024-03-17T21:57:56.253425-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\
  \u0E17\u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E1E\u0E1A\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E41\u0E25\u0E30\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\
  \u0E07\u0E21\u0E31\u0E19\u0E2D\u0E2D\u0E01\u0E44\u0E1B\u0E44\u0E14\u0E49 \u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E21\
  \u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2D\u0E31\u0E1B\u0E40\u0E14\u0E15\u0E42\
  \u0E04\u0E49\u0E14, \u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07, \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\
  \u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\
  \u0E14\u0E40\u0E23\u0E47\u0E27."
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 10
---

## อะไรและทำไม?
การค้นหาและแทนที่ข้อความช่วยให้คุณพบสตริงและเปลี่ยนแปลงมันออกไปได้ โปรแกรมเมอร์ใช้มันเพื่ออัปเดตโค้ด, ปรับโครงสร้าง, หรือเปลี่ยนข้อมูลอย่างรวดเร็ว

## วิธีการ:

มาค้นหาและแทนที่ข้อความโดยใช้ Haskell กัน เราจะใช้ `Data.Text` สำหรับการจัดการข้อความ Unicode และเพื่อประสิทธิภาพ ตรวจสอบให้แน่ใจว่าได้นำเข้า `Data.Text` ดังนี้:

```haskell
import qualified Data.Text as T
```

ตอนนี้, มาแทนที่ทุกตัวอย่างของ "hello" ด้วย "hi" ในข้อความ:

```haskell
replaceText :: T.Text -> T.Text -> T.Text -> T.Text
replaceText old new = T.replace old new

main :: IO ()
main = do
  let originalText = T.pack "hello world, hello Haskell!"
  let newText = replaceText (T.pack "hello") (T.pack "hi") originalText
  print newText -- "hi world, hi Haskell!"
```

ฟังก์ชัน `replace` ทำงานหนัก ได้ทำการหุ้มมันใน `replaceText` เพื่อความชัดเจน

## ขุดลึก

ฟังก์ชันการแทนที่ข้อความของ Haskell เช่น `T.replace` นั้นสร้างขึ้นบนความสามารถในการประมวลผลอาร์เรย์ของ Haskell. ย้อนกลับไป Haskell ถูกสร้างขึ้นในยุค 80 กับการเน้นการเขียนโปรแกรมแบบฟังก์ชัน ซึ่งทำให้การดำเนินการเช่นการแทนที่ข้อความมีความสง่างามและมีข้อผิดพลาดน้อยลงเนื่องจากความไม่เปลี่ยนแปลงและระบบประเภทที่แข็งแรง

จากทางเลือกอื่น, คุณอาจได้ทำการวนซ้ำผ่านข้อความและแทนที่ย่อยด้วยตนเอง แต่นั่นมีโอกาสผิดพลาดมากขึ้นและไม่มีประสิทธิภาพ

ไลบรารี `Data.Text` ใช้การแทนตัวแรกที่ต่างกันจากประเภท `String` (ซึ่งเป็นเพียงรายการของอักขระ), ทำให้มันเหมาะสมกว่าสำหรับการดำเนินการข้อความขนาดใหญ่ ฟังก์ชัน `T.replace` เองใช้อัลกอริทึมที่มีประสิทธิภาพสำหรับการค้นหาสตริง ซึ่งให้ประสิทธิภาพที่ดียาวนานแม้กระทั่งสำหรับข้อความขนาดใหญ่

## ดูด้วย

สำหรับข้อมูลเพิ่มเติมเกี่ยวกับ `Data.Text`, ตรวจสอบที่:

- [แพคเกจ Text บน Hackage](https://hackage.haskell.org/package/text)

ยังสามารถพิจารณาการอ่านเพิ่มเติมเกี่ยวกับการจัดการสตริงของ Haskell:

- [Haskell Wiki เกี่ยวกับสตริง](https://wiki.haskell.org/Strings)
- [Learn You a Haskell for Great Good! เกี่ยวกับข้อความ](http://learnyouahaskell.com/input-and-output#files-and-streams)

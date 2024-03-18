---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:32.857292-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\
  \u0E25\u0E02\u0E2A\u0E38\u0E48\u0E21\u0E43\u0E19 Haskell \u0E21\u0E35\u0E04\u0E27\
  \u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E17\u0E35\u0E48\u0E04\u0E32\
  \u0E14\u0E40\u0E14\u0E32\u0E44\u0E21\u0E48\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E21\
  \u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E02\u0E2D\u0E07\u0E21\u0E19\u0E38\u0E29\u0E22\
  \u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.264108-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\
  \u0E25\u0E02\u0E2A\u0E38\u0E48\u0E21\u0E43\u0E19 Haskell \u0E21\u0E35\u0E04\u0E27\
  \u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E17\u0E35\u0E48\u0E04\u0E32\
  \u0E14\u0E40\u0E14\u0E32\u0E44\u0E21\u0E48\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E21\
  \u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E02\u0E2D\u0E07\u0E21\u0E19\u0E38\u0E29\u0E22\
  \u0E4C\u2026"
title: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\u0E25\
  \u0E02\u0E2A\u0E38\u0E48\u0E21"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การสร้างตัวเลขสุ่มใน Haskell มีความหมายถึงการสร้างตัวเลขที่คาดเดาไม่ได้โดยมาตรฐานของมนุษย์ นี่เป็นสิ่งสำคัญในการใช้งานที่หลากหลายตั้งแต่แอปพลิเคชั่นด้านการเข้ารหัสไปจนถึงการจำลองสถานการณ์ที่ต้องการองค์ประกอบของโอกาสเพื่อจำลองปรากฏการณ์ในโลกแห่งความจริงได้อย่างแม่นยำ

## วิธีทำ:

ในการสร้างตัวเลขสุ่มใน Haskell, ตัวเลือกหนึ่งที่ปกติจะใช้ก็คือแพคเกจ `random` ซึ่งเป็นส่วนหนึ่งของ Haskell Platform ดังนี้คือคู่มือเป็นขั้นตอน:

แรก, ตรวจสอบว่าคุณได้ติดตั้งแพคเกจ `random` แล้วหรือไม่ หากไม่ได้ติดตั้ง คุณสามารถหามันผ่าน Cabal หรือ Stack

### การสร้างตัวเลขสุ่ม

ในการสร้างตัวเลขสุ่มง่ายๆ คุณสามารถใช้ฟังก์ชัน `randomRIO` ซึ่งสร้างค่าสุ่มภายในช่วงที่กำหนด

```Haskell
import System.Random (randomRIO)

main :: IO ()
main = do
  randomNumber <- randomRIO (1, 10) :: IO Int
  putStrLn $ "Random number: " ++ show randomNumber
```

### การสร้างรายการของตัวเลขสุ่ม

การสร้างรายการของตัวเลขสุ่มเป็นเรื่องที่ซับซ้อนขึ้นเล็กน้อยแต่ยังคงเป็นเรื่องง่ายต่อการทำ:

```Haskell
import System.Random (randomRIO)

randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n = do
  r <- randomRIO (1, 100)
  rs <- randomList (n-1)
  return (r:rs)

main :: IO ()
main = do
  numbers <- randomList 5
  print numbers
```

ส่วนของโค้ดนี้สร้างฟังก์ชัน `randomList` ที่สร้างรายการของตัวเลขเต็มสุ่ม แทนที่ `(1, 100)` ด้วยช่วงที่คุณต้องการ

## ลงลึก

แพคเกจ `random` ของ Haskell นั้นให้เครื่องกำเนิดตัวเลขเทียมสุ่ม (PRNG), ซึ่งหมายความว่าตัวเลขที่สร้างจะไม่ใช่สุ่มแท้จริง แต่สามารถดูเหมือนว่าเป็นสุ่มสำหรับการใช้งานหลายๆ อย่าง จุดแข็งหลักของความสามารถในการสร้างตัวเลขสุ่มของ Haskell นั้นอยู่ที่ Type class `RandomGen`, ซึ่งนำเสนอวิธีการที่แตกต่างกันในการสร้างตัวเลขสุ่ม, และ Type class `Random`, ซึ่งรวมถึงประเภทที่สามารถสร้างแบบสุ่มได้

จากอดีต, การจัดการสร้างตัวเลขสุ่มของ Haskell มุ่งเน้นไปที่ความสะอาดและการสามารถทำซ้ำได้ เนื่องจากการดำเนินการที่เกี่ยวกับความไม่แน่นอนถูกจัดการอย่างชัดเจนในมอนาด `IO` หรือต้องการการส่งและอัปเดตสถานะของเครื่องกำเนิดโดยตรง - เพื่อรักษาความโปร่งใสในการอ้างอิง

ในการใช้งานบางอย่าง เช่น การเข้ารหัส ตัวเลขเทียมสุ่มที่สร้างโดย PRNG มาตรฐานอาจไม่มีความปลอดภัยพอ ในกรณีเหล่านี้ โปรแกรมเมอร์ Haskell มักจะหันไปใช้ไลบรารีที่เชี่ยวชาญเช่น `crypto-random`, ซึ่งถูกออกแบบมาเพื่อตอบสนองความต้องการที่เข้มงวดของการใช้งานด้านการเข้ารหัส

นอกจากนี้, ไลบรารีอื่นๆ เช่น `mwc-random` เสนอประสิทธิภาพและคุณภาพของตัวเลขสุ่มที่ดีกว่าสำหรับจำลองการทำงานและการใช้งานอื่นๆ โดยการนำเอาอัลกอริทึมที่ทันสมัยเช่น Mersenne Twister มาใช้งาน 

เมื่อเลือกวิธีการสร้างตัวเลขสุ่มใน Haskell, จำเป็นต้องพิจารณาความต้องการของการใช้งานเกี่ยวกับคุณภาพของความสุ่ม, ประสิทธิภาพ, และความปลอดภัยเพื่อเลือกเครื่องมือหรือไลบรารีที่เหมาะสมที่สุด
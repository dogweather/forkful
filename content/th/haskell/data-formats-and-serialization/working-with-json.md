---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:52.854032-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Haskell \u0E44\u0E21\u0E48\
  \u0E21\u0E35\u0E01\u0E32\u0E23\u0E23\u0E2D\u0E07\u0E23\u0E31\u0E1A JSON \u0E43\u0E19\
  \u0E15\u0E31\u0E27\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A JavaScript\
  \ \u0E41\u0E15\u0E48\u0E14\u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E0A\u0E48\u0E27\
  \u0E22\u0E40\u0E2B\u0E25\u0E37\u0E2D\u0E08\u0E32\u0E01\u0E44\u0E25\u0E1A\u0E23\u0E32\
  \u0E23\u0E35\u0E02\u0E2D\u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\u0E17\u0E35\u0E48\u0E2A\
  \u0E32\u0E21 \u0E40\u0E0A\u0E48\u0E19 **Aeson** \u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\
  \u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A JSON\u2026"
lastmod: '2024-04-05T21:54:02.005562-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u0E44\u0E21\u0E48\u0E21\u0E35\u0E01\u0E32\u0E23\u0E23\u0E2D\u0E07\
  \u0E23\u0E31\u0E1A JSON \u0E43\u0E19\u0E15\u0E31\u0E27\u0E40\u0E2B\u0E21\u0E37\u0E2D\
  \u0E19\u0E01\u0E31\u0E1A JavaScript \u0E41\u0E15\u0E48\u0E14\u0E49\u0E27\u0E22\u0E01\
  \u0E32\u0E23\u0E0A\u0E48\u0E27\u0E22\u0E40\u0E2B\u0E25\u0E37\u0E2D\u0E08\u0E32\u0E01\
  \u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E02\u0E2D\u0E07\u0E1A\u0E38\u0E04\u0E04\
  \u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21 \u0E40\u0E0A\u0E48\u0E19 **Aeson** \u0E01\
  \u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A JSON \u0E08\u0E36\
  \u0E07\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\
  \u0E22 Aeson \u0E43\u0E2B\u0E49\u0E17\u0E31\u0E49\u0E07\u0E1F\u0E31\u0E07\u0E01\u0E4C\
  \u0E0A\u0E31\u0E19\u0E23\u0E30\u0E14\u0E31\u0E1A\u0E2A\u0E39\u0E07\u0E41\u0E25\u0E30\
  \u0E23\u0E30\u0E14\u0E31\u0E1A\u0E15\u0E48\u0E33\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E23\u0E2B\u0E31\u0E2A (\u0E41\u0E1B\
  \u0E25\u0E07\u0E04\u0E48\u0E32\u0E02\u0E2D\u0E07 Haskell \u0E40\u0E1B\u0E47\u0E19\
  \ JSON) \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E16\u0E2D\u0E14\u0E23\u0E2B\u0E31\
  \u0E2A (\u0E41\u0E1B\u0E25\u0E07 JSON \u0E40\u0E1B\u0E47\u0E19\u0E04\u0E48\u0E32\
  \u0E02\u0E2D\u0E07 Haskell)."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
weight: 38
---

## วิธีการ:
Haskell ไม่มีการรองรับ JSON ในตัวเหมือนกับ JavaScript แต่ด้วยการช่วยเหลือจากไลบรารีของบุคคลที่สาม เช่น **Aeson** การจัดการกับ JSON จึงเป็นเรื่องง่าย Aeson ให้ทั้งฟังก์ชันระดับสูงและระดับต่ำสำหรับการเข้ารหัส (แปลงค่าของ Haskell เป็น JSON) และการถอดรหัส (แปลง JSON เป็นค่าของ Haskell)

### การติดตั้ง Aeson
ก่อนอื่น เพิ่ม Aeson เข้าไปใน dependencies ของโปรเจกต์ของคุณโดยการอัปเดตไฟล์ `.cabal` หรือใช้ Stack หรือ Cabal โดยตรง:

```shell
cabal update && cabal install aeson
```
หรือ หากคุณใช้ Stack:
```shell
stack install aeson
```

### การถอดรหัส JSON
เรามาเริ่มต้นด้วยตัวอย่างเบื้องต้นของการถอดรหัสข้อมูล JSON เป็นประเภทข้อมูลของ Haskell สมมติว่าเรามี JSON ต่อไปนี้ที่แสดงถึงคนหนึ่งคน:

```json
{
  "name": "John Doe",
  "age": 30
}
```

ก่อนอื่น กำหนดประเภทข้อมูล Haskell ที่สอดคล้องกันและทำให้มันเป็น instance ของ `FromJSON`:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, Show)

instance FromJSON Person

-- ฟังก์ชันสำหรับการถอดรหัส JSON จากไฟล์
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
การใช้งาน:
สมมติว่า `person.json` ประกอบด้วยข้อมูล JSON ดังที่แสดงข้างต้น ให้รัน:
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
ผลลัพธ์ตัวอย่าง:
```haskell
Just (Person {name = "John Doe", age = 30})
```

### การเข้ารหัสค่าของ Haskell เป็น JSON
เพื่อแปลงค่าของ Haskell กลับสู่ JSON คุณจำเป็นต้องทำให้ประเภทของคุณเป็น instance ของ `ToJSON` จากนั้นใช้ `encode`.

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- สมมุติประเภท Person จากก่อนหน้า

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
ผลลัพธ์ตัวอย่าง:
```json
{"name":"Jane Doe","age":32}
```

ตัวอย่างเหล่านี้แสดงให้เห็นพื้นฐานของการทำงานกับ JSON ใน Haskell โดยใช้ Aeson จำไว้ว่า Aeson มีอีกมากมาย รวมถึงกฎการแปลงที่กำหนดเอง การทำงานกับ JSON ที่มีโครงสร้างซับซ้อน และอื่นๆ อีกมากที่เหมาะสำหรับความต้องการและสถานการณ์ที่หลากหลาย

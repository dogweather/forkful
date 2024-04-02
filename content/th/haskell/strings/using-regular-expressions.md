---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:18.336439-06:00
description: "\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\u0E1B\u0E23\u0E01\u0E15\u0E34\u0E43\
  \u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E04\u0E37\u0E2D\u0E25\u0E33\u0E14\u0E31\u0E1A\u0E02\u0E2D\u0E07\u0E2D\
  \u0E31\u0E01\u0E02\u0E23\u0E30\u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E23\
  \u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32 \u0E42\
  \u0E14\u0E22\u0E1B\u0E01\u0E15\u0E34\u0E08\u0E30\u0E43\u0E0A\u0E49\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E2A\u0E15\u0E23\u0E34\u0E07 Haskell\u2026"
lastmod: '2024-03-17T21:57:56.258365-06:00'
model: gpt-4-0125-preview
summary: "\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\u0E1B\u0E23\u0E01\u0E15\u0E34\u0E43\
  \u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E04\u0E37\u0E2D\u0E25\u0E33\u0E14\u0E31\u0E1A\u0E02\u0E2D\u0E07\u0E2D\
  \u0E31\u0E01\u0E02\u0E23\u0E30\u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E23\
  \u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32 \u0E42\
  \u0E14\u0E22\u0E1B\u0E01\u0E15\u0E34\u0E08\u0E30\u0E43\u0E0A\u0E49\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E2A\u0E15\u0E23\u0E34\u0E07 Haskell\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 regular expressions"
weight: 11
---

## อะไร & ทำไม?
นิพจน์ปรกติในการเขียนโปรแกรมคือลำดับของอักขระที่กำหนดรูปแบบการค้นหา โดยปกติจะใช้สำหรับการค้นหาและจัดการสตริง Haskell โปรแกรมเมอร์ใช้นิพจน์ปรกติสำหรับงานตั้งแต่การจับคู่สตริงง่ายๆ ไปจนถึงการประมวลผลข้อความที่ซับซ้อน โดยใช้ประโยชน์จากความเร็วและความหลากหลายในการจัดการข้อมูลข้อความ

## วิธีการ:
ใน Haskell, ฟังก์ชันการทำงานของ regex ไม่ได้เป็นส่วนหนึ่งของไลบรารีมาตรฐาน ซึ่งจำเป็นต้องใช้แพ็กเกจของบุคคลที่สาม เช่น `regex-base` ร่วมกับแบ็กเอ็นด์ที่เข้ากันได้ เช่น `regex-posix` (สำหรับการสนับสนุน regex POSIX), `regex-pcre` (สำหรับ regex ที่เข้ากันได้กับ Perl), ฯลฯ นี่คือวิธีที่คุณสามารถใช้แพ็กเกจเหล่านี้เพื่อทำงานกับนิพจน์ปรกติ

เริ่มแรก, ตรวจสอบให้แน่ใจว่าคุณมีแพ็กเกจที่ติดตั้งโดยการเพิ่ม `regex-posix` หรือ `regex-pcre` ไปยังไฟล์ `.cabal` ของโปรเจกต์ หรือการติดตั้งผ่าน cabal โดยตรง:

```bash
cabal install regex-posix
```
หรือ
```bash
cabal install regex-pcre
```

### การใช้ `regex-posix`:

```haskell
import Text.Regex.Posix ((=~))

-- ตรวจสอบว่าสตริงตรงกับรูปแบบหรือไม่
isMatch :: String -> String -> Bool
isMatch text pattern = text =~ pattern :: Bool

-- ค้นหาการตรงกันครั้งแรก
findFirst :: String -> String -> String
findFirst text pattern = text =~ pattern :: String

main :: IO ()
main = do
    print $ isMatch "hello world" "wo"
    -- ผลลัพธ์: True
    print $ findFirst "good morning, good night" "good"
    -- ผลลัพธ์: "good"
```

### การใช้ `regex-pcre`:

```haskell
import Text.Regex.PCRE ((=~))

-- ค้นหาการตรงกันทั้งหมด
findAll :: String -> String -> [String]
findAll text pattern = text =~ pattern :: [String]

main :: IO ()
main = do
    print $ findAll "test1 test2 test3" "\\btest[0-9]\\b"
    -- ผลลัพธ์: ["test1","test2","test3"]
```

แต่ละไลบรารีมีคุณลักษณะเฉพาะ แต่วิธีการใช้ `=~` เพื่อใช้งาน regex ยังคงเหมือนกัน ไม่ว่าคุณจะตรวจสอบการตรงกันหรือสกัดสตริงย่อย การเลือกใช้ระหว่าง `regex-posix` หรือ `regex-pcre` ขึ้นอยู่กับความต้องการของโปรเจกต์ของคุณและความสามารถเฉพาะด้าน regex ที่ต้องการ

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:56.941832-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E19 Haskell \u0E2B\u0E21\u0E32\
  \u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2B\u0E23\
  \u0E37\u0E2D\u0E2D\u0E31\u0E1B\u0E40\u0E14\u0E15\u0E44\u0E1F\u0E25\u0E4C\u0E17\u0E35\
  \u0E48\u0E21\u0E35\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E40\u0E1B\u0E47\u0E19\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u2026"
lastmod: '2024-03-17T21:57:56.286199-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E19 Haskell \u0E2B\u0E21\u0E32\
  \u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2B\u0E23\
  \u0E37\u0E2D\u0E2D\u0E31\u0E1B\u0E40\u0E14\u0E15\u0E44\u0E1F\u0E25\u0E4C\u0E17\u0E35\
  \u0E48\u0E21\u0E35\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E40\u0E1B\u0E47\u0E19\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การเขียนไฟล์ข้อความใน Haskell หมายถึงการสร้างหรืออัปเดตไฟล์ที่มีเนื้อหาเป็นข้อความโดยใช้โปรแกรม โปรแกรมเมอร์ทำสิ่งนี้เพื่อเก็บข้อมูลเช่นข้อความบันทึกการทำงาน, ผลลัพธ์จากแอปพลิเคชัน, หรือเก็บเนื้อหาที่สร้างโดยผู้ใช้ ซึ่งเป็นงานพื้นฐานสำหรับแอปพลิเคชันที่ต้องการการเก็บข้อมูลระยะยาวหรือการบันทึก

## วิธีการ:

พรีลูดของ Haskell ให้การสนับสนุนเบื้องต้นสำหรับการเขียนไฟล์โดยใช้ฟังก์ชัน `writeFile` และ `appendFile` จากโมดูล `System.IO` นี่คือตัวอย่างพื้นฐานในการสร้างไฟล์ใหม่ (หรือเขียนทับไฟล์ที่มีอยู่) และจากนั้นก็เพิ่มข้อความลงไปในไฟล์

```haskell
import System.IO

-- เขียนไฟล์, เขียนทับหากมีอยู่
main :: IO ()
main = do
  writeFile "example.txt" "This is line one.\n"
  appendFile "example.txt" "This is line two.\n"
```

เมื่อคุณรันโปรแกรมนี้, มันจะสร้าง (หรือลบ) `example.txt` และเขียน "This is line one." ตามด้วย "This is line two." บนบรรทัดถัดไป

สำหรับการจัดการไฟล์ที่ซับซ้อนยิ่งขึ้น, โปรแกรมเมอร์ Haskell มักจะหันไปใช้แพ็กเกจ `text` เพื่อการประมวลผลสตริงอย่างมีประสิทธิภาพ และแพ็กเกจ `bytestring` สำหรับการจัดการข้อมูลแบบไบนารี นี่คือวิธีการใช้แพ็กเกจ `text` สำหรับอินพุตเอาต์พุตแบบไฟล์:

ก่อนอื่น, คุณต้องเพิ่ม `text` เข้าไปในการพึ่งพาโปรเจ็กต์ของคุณ จากนั้น, คุณสามารถใช้มันดังต่อไปนี้:

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- เขียนไฟล์โดยใช้แพ็กเกจ text
main :: IO ()
main = do
  let content = T.pack "Using the text package for better performance.\n"
  TIO.writeFile "textExample.txt" content
  TIO.appendFile "textExample.txt" $ T.pack "Appending line two.\n"
```

ในส่วนนี้ `T.pack` แปลง `String` ปกติเป็นประเภท `Text` ซึ่งมีประสิทธิภาพมากขึ้น `TIO.writeFile` และ `TIO.appendFile` คือความเทียบเท่าของ `text` สำหรับการเขียนและเพิ่มไฟล์ตามลำดับ

การรันโค้ดนี้จะส่งผลให้ไฟล์ชื่อ `textExample.txt` โดยมีข้อความสองบรรทัด โดยสาธิตการสร้างและเพิ่มข้อความโดยใช้ห้องสมุด `text` สำหรับประสิทธิภาพที่ดีกว่าและความสามารถในการจัดการข้อความ Unicode

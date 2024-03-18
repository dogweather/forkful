---
changelog:
- 2024-01-21, dogweather, Reviewed for accuracy
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:21.876574-06:00
description: "\u0E17\u0E38\u0E01\u0E46 \u0E42\u0E1B\u0E23\u0E40\u0E08\u0E47\u0E01\u0E15\
  \u0E4C\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E14\u0E49\u0E27\u0E22\u0E02\
  \u0E31\u0E49\u0E19\u0E15\u0E2D\u0E19\u0E41\u0E23\u0E01\u0E40\u0E1E\u0E35\u0E22\u0E07\
  \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\u0E14\u0E35\u0E22\u0E27 \u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  , \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\
  \u0E07\u0E04\u0E48\u0E32\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E40\
  \u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\
  \u0E19\u0E42\u0E04\u0E49\u0E14\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.268926-06:00'
model: gpt-4-0125-preview
summary: "\u0E17\u0E38\u0E01\u0E46 \u0E42\u0E1B\u0E23\u0E40\u0E08\u0E47\u0E01\u0E15\
  \u0E4C\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E14\u0E49\u0E27\u0E22\u0E02\
  \u0E31\u0E49\u0E19\u0E15\u0E2D\u0E19\u0E41\u0E23\u0E01\u0E40\u0E1E\u0E35\u0E22\u0E07\
  \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\u0E14\u0E35\u0E22\u0E27 \u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  , \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\
  \u0E07\u0E04\u0E48\u0E32\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E40\
  \u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\
  \u0E19\u0E42\u0E04\u0E49\u0E14\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u2026"
title: "\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\u0E04\u0E23\u0E07\u0E01\
  \u0E32\u0E23\u0E43\u0E2B\u0E21\u0E48"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
ทุกๆ โปรเจ็กต์เริ่มต้นด้วยขั้นตอนแรกเพียงอย่างเดียว สำหรับโปรแกรมเมอร์, หมายถึงการตั้งค่าโครงสร้างเริ่มต้นและเขียนโค้ดเริ่มต้น เราทำสิ่งนี้เพื่อเปลี่ยนความคิดให้กลายเป็นฐานที่มั่นคง พร้อมสำหรับการขยายและนวัตกรรม

## วิธีทำ:
```Haskell
-- 1. เริ่มต้นโปรเจ็กต์ Haskell ใหม่โดยใช้ Stack
$ stack new myproject

-- คำสั่งข้างต้นจะสร้างไดเรกทอรีใหม่ `myproject` พร้อมไฟล์บางไฟล์:
-- myproject/
-- ├── app/
-- │   └── Main.hs        # ไฟล์โปรแกรมหลัก Main ของคุณ
-- ├── src/               # ไฟล์ต้นฉบับสำหรับไลบรารี
-- ├── test/              # ไฟล์ทดสอบ
-- ├── myproject.cabal    # ไฟล์อธิบายแพคเกจ
-- ├── stack.yaml         # การตั้งค่า Stack
-- └── Setup.hs           # สคริปต์การตั้งค่าการสร้าง

-- 2. การสร้างโปรเจ็กต์
$ cd myproject
$ stack build

-- 3. การรันโปรเจ็กต์ Haskell ใหม่ของคุณ
$ stack run

-- ผลลัพธ์ตัวอย่าง:
someFunc
```

## ลงลึก
โปรเจ็กต์ Haskell มักพึ่งพาเครื่องมือเช่น Stack หรือ Cabal Stack จัดการการพึ่งพา, รับประกันการสร้างที่สม่ำเสมอ ในปี 2008, Stack เป็นการเปลี่ยนแปลงที่สำคัญสำหรับ Haskell, แก้ไขข้อบกพร่องของ Cabal เกี่ยวกับความขัดแย้งของแพคเกจ

ทางเลือกอื่น ๆ ได้แก่ การใช้ Cabal อย่างเดียว หรือเครื่องมือใหม่ๆ เช่น GHCup หรือ Nix เพื่อการสร้างที่สามารถทำซ้ำได้ คุณอาจเลือกใช้ Cabal เพื่อความเรียบง่าย หรือ Nix เมื่องานของคุณต้องการความสามารถในการทำซ้ำ แต่ Stack ให้สมดุลที่น่าพอใจสำหรับหลายๆ อย่าง

ที่ซ่อนอยู่ภายใต้, `stack new` ใช้เทมเพลตในการสร้างโครงสร้างโปรเจ็กต์ ประกอบด้วยไม่เพียงแต่โค้ดต้นฉบับของคุณ แต่ยังรวมถึงการกำหนดค่าสำหรับการสร้างและการพึ่งพา ไฟล์ `.cabal` เป็นสิ่งสำคัญที่มีข้อมูลเมตาและคำสั่งการสร้าง

## ดูเพิ่มเติม
- เรียนรู้เพิ่มเติมเกี่ยวกับ Stack: [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
- ดำดิ่งลงไปใน Cabal: [The Haskell Cabal](https://www.haskell.org/cabal/users-guide/)

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:14.456758-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Haskell \u0E23\u0E2D\u0E07\
  \u0E23\u0E31\u0E1A\u0E01\u0E23\u0E2D\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E2B\u0E25\u0E32\u0E22\u0E2D\u0E22\u0E48\u0E32\u0E07, \u0E41\u0E15\u0E48\
  \u0E2A\u0E2D\u0E07\u0E2D\u0E31\u0E19\u0E17\u0E35\u0E48\u0E44\u0E14\u0E49\u0E23\u0E31\
  \u0E1A\u0E04\u0E27\u0E32\u0E21\u0E19\u0E34\u0E22\u0E21\u0E04\u0E37\u0E2D `Hspec`\
  \ \u0E41\u0E25\u0E30 `QuickCheck` Hspec\u2026"
lastmod: '2024-04-05T21:54:01.974644-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u0E23\u0E2D\u0E07\u0E23\u0E31\u0E1A\u0E01\u0E23\u0E2D\u0E1A\u0E01\
  \u0E32\u0E23\u0E17\u0E14\u0E2A\u0E2D\u0E1A\u0E2B\u0E25\u0E32\u0E22\u0E2D\u0E22\u0E48\
  \u0E32\u0E07, \u0E41\u0E15\u0E48\u0E2A\u0E2D\u0E07\u0E2D\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E44\u0E14\u0E49\u0E23\u0E31\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E19\u0E34\u0E22\
  \u0E21\u0E04\u0E37\u0E2D `Hspec` \u0E41\u0E25\u0E30 `QuickCheck` Hspec \u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E02\
  \u0E49\u0E2D\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\
  \u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E04\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13, \u0E43\u0E19\u0E02\
  \u0E13\u0E30\u0E17\u0E35\u0E48 QuickCheck \u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\
  \u0E23\u0E49\u0E32\u0E07\u0E01\u0E32\u0E23\u0E17\u0E14\u0E2A\u0E2D\u0E1A\u0E42\u0E14\
  \u0E22\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34\u0E42\u0E14\u0E22\u0E2D\
  \u0E18\u0E34\u0E1A\u0E32\u0E22\u0E04\u0E38\u0E13\u0E2A\u0E21\u0E1A\u0E31\u0E15\u0E34\
  \u0E17\u0E35\u0E48\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E04\
  \u0E27\u0E23\u0E1E\u0E36\u0E07\u0E1E\u0E2D\u0E43\u0E08."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E01\u0E32\u0E23\u0E17\u0E14\
  \u0E2A\u0E2D\u0E1A"
weight: 36
---

## วิธีการ:
Haskell รองรับกรอบการทดสอบหลายอย่าง, แต่สองอันที่ได้รับความนิยมคือ `Hspec` และ `QuickCheck` Hspec ช่วยให้คุณกำหนดข้อกำหนดที่อ่านได้โดยคนสำหรับโค้ดของคุณ, ในขณะที่ QuickCheck ให้คุณสร้างการทดสอบโดยอัตโนมัติโดยอธิบายคุณสมบัติที่โค้ดของคุณควรพึงพอใจ

### การใช้ Hspec
ก่อนอื่น, อย่าลืมเพิ่ม `hspec` ลงในการกำหนดค่าเครื่องมือสร้างของคุณ (เช่น, `stack.yaml` หรือไฟล์ `cabal`) จากนั้น, นำเข้า `Test.Hspec` และเขียนทดสอบเป็นข้อกำหนด:

```haskell
-- ไฟล์: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "บวกเลขสองตัว" $
    add 1 2 `shouldBe` 3

  it "คืนค่าตัวเลขแรกเมื่อบวกกับศูนย์" $
    add 5 0 `shouldBe` 5
```

จากนั้น, รันการทดสอบของคุณโดยใช้เครื่องมือสร้างของคุณ, ผลลัพธ์อาจจะดูออกมาเป็น:

```
MyLib.add
  - บวกเลขสองตัว
  - คืนค่าตัวเลขแรกเมื่อบวกกับศูนย์

ทำเสร็จใน 0.0001 วินาที
2 ตัวอย่าง, 0 ความล้มเหลว
```

### การใช้ QuickCheck
กับ QuickCheck, คุณแสดงคุณสมบัติที่ฟังก์ชั่นของคุณควรพึงพอใจ ใส่ `QuickCheck` ลงในการกำหนดค่าโครงการของคุณ, จากนั้นนำเข้ามัน:

```haskell
-- ไฟล์: test/MyLibProperties.hs
import Test.QuickCheck
import MyLib (add)

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = x + (y + z) == (x + y) + z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck prop_addAssociative
  quickCheck prop_addCommutative
```

การรันการทดสอบเหล่านี้จะสร้างอินพุตโดยอัตโนมัติเพื่อตรวจสอบคุณสมบัติที่ระบุ:

```
+++ OK, ผ่าน 100 การทดสอบ.
+++ OK, ผ่าน 100 การทดสอบ.
```

ในทั้ง Hspec และ QuickCheck ตัวอย่าง, ชุดการทดสอบทำหน้าที่เป็นเอกสารที่สามารถรันได้ซึ่งสามารถตรวจสอบความถูกต้องของโค้ดของคุณได้อย่างอัตโนมัติ

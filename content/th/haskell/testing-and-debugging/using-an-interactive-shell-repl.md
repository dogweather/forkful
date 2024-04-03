---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:37.720138-06:00
description: "Shell \u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E42\u0E15\
  \u0E49\u0E15\u0E2D\u0E1A\u0E44\u0E14\u0E49 \u0E2B\u0E23\u0E37\u0E2D REPL (Read-Eval-Print\
  \ Loop) \u0E43\u0E19 Haskell \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\
  \u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E23\u0E31\u0E19\u0E42\u0E04\u0E49\u0E14\
  \u0E2A\u0E48\u0E27\u0E19\u0E40\u0E25\u0E47\u0E01\u0E46\u0E44\u0E14\u0E49\u0E41\u0E1A\
  \u0E1A\u0E2A\u0E14\u0E46\u2026"
lastmod: '2024-03-17T21:57:56.269898-06:00'
model: gpt-4-0125-preview
summary: "Shell \u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E42\u0E15\
  \u0E49\u0E15\u0E2D\u0E1A\u0E44\u0E14\u0E49 \u0E2B\u0E23\u0E37\u0E2D REPL (Read-Eval-Print\
  \ Loop) \u0E43\u0E19 Haskell \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\
  \u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E23\u0E31\u0E19\u0E42\u0E04\u0E49\u0E14\
  \u0E2A\u0E48\u0E27\u0E19\u0E40\u0E25\u0E47\u0E01\u0E46\u0E44\u0E14\u0E49\u0E41\u0E1A\
  \u0E1A\u0E2A\u0E14\u0E46 \u0E21\u0E31\u0E19\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E19\u0E32\
  \u0E21\u0E40\u0E14\u0E47\u0E01\u0E40\u0E25\u0E48\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\
  \u0E1A\u0E01\u0E32\u0E23\u0E44\u0E14\u0E49\u0E23\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E40\
  \u0E2A\u0E19\u0E2D\u0E41\u0E19\u0E30\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\
  \u0E40\u0E23\u0E47\u0E27 \u0E17\u0E14\u0E2A\u0E2D\u0E1A\u0E1F\u0E31\u0E07\u0E01\u0E4C\
  \u0E0A\u0E31\u0E19 \u0E41\u0E25\u0E30\u0E40\u0E23\u0E35\u0E22\u0E19\u0E23\u0E39\u0E49\
  \u0E20\u0E32\u0E29\u0E32."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
weight: 34
---

## วิธีการ:
เพื่อเริ่มต้นใช้งาน GHCi (Glasgow Haskell Compiler's interactive environment) เพียงพิมพ์ `ghci` ในเทอร์มินัลของคุณ นี่คือวิธีการใช้:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

ผลลัพธ์ตัวอย่างอธิบายว่า `x` เป็นตัวแปรตัวเลขและแสดงให้เห็นว่าการคูณสองเท่าของมันจะได้ผลลัพธ์เป็น 10

## การศึกษาลึก:
GHCi ของ Haskell มีการพัฒนามาอย่างยาวนานนับตั้งแต่เริ่มต้น มันมีชุดคุณสมบัติที่หลากหลาย เช่น การเสร็จสิ้นด้วยแท็บ, การป้อนหลายบรรทัด, และการโหลดแพ็คเกจ ทางเลือกอื่นๆ เช่น Hugs นั้นส่วนใหญ่เป็นประวัติศาสตร์แล้ว โดย GHCi เป็นมาตรฐาน GHCi คอมไพล์โค้ดในขณะที่คุณป้อนนิพจน์แต่ละครั้ง ให้วิธีที่มีประสิทธิภาพในการทดสอบโค้ด Haskell ของคุณ

## ดูเพิ่มเติม:
- [คู่มือผู้ใช้ GHC – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [เรียน Haskell อย่างสนุกสนาน! – เริ่มต้น](http://learnyouahaskell.com/starting-out#hello-world)
- [วิกิ Haskell – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:55.393415-06:00
description: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E40\u0E0A\u0E25\
  \u0E25\u0E4C\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E2B\u0E23\u0E37\u0E2D Read-Eval-Print\
  \ Loop (REPL) \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E40\
  \u0E02\u0E35\u0E22\u0E19\u0E42\u0E04\u0E49\u0E14\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\
  \u0E32\u0E07\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E43\u0E19\
  \u0E01\u0E32\u0E23\u0E17\u0E14\u0E25\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14\u0E2A\u0E48\
  \u0E27\u0E19\u0E40\u0E25\u0E47\u0E01\u0E46 \u0E02\u0E2D\u0E07 Swift \u0E2D\u0E22\
  \u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27,\u2026"
lastmod: '2024-03-17T21:57:56.563366-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E40\u0E0A\u0E25\
  \u0E25\u0E4C\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E2B\u0E23\u0E37\u0E2D Read-Eval-Print\
  \ Loop (REPL) \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E40\
  \u0E02\u0E35\u0E22\u0E19\u0E42\u0E04\u0E49\u0E14\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\
  \u0E32\u0E07\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E43\u0E19\
  \u0E01\u0E32\u0E23\u0E17\u0E14\u0E25\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14\u0E2A\u0E48\
  \u0E27\u0E19\u0E40\u0E25\u0E47\u0E01\u0E46 \u0E02\u0E2D\u0E07 Swift \u0E2D\u0E22\
  \u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27, \u0E01\u0E32\u0E23\
  \u0E14\u0E35\u0E1A\u0E31\u0E01, \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E23\u0E35\u0E22\
  \u0E19\u0E23\u0E39\u0E49\u0E20\u0E32\u0E29\u0E32."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
weight: 34
---

## อะไรและทำไม?
การใช้งานเชลล์โต้ตอบหรือ Read-Eval-Print Loop (REPL) ช่วยให้คุณเขียนโค้ดได้อย่างโต้ตอบ โปรแกรมเมอร์ใช้มันในการทดลองโค้ดส่วนเล็กๆ ของ Swift อย่างรวดเร็ว, การดีบัก, หรือเรียนรู้ภาษา

## วิธีการ:
เรียกใช้งาน REPL โดยการเปิดเทอร์มินัลและรัน `swift` พิมพ์โค้ดโดยตรงและกด Enter เพื่อรันมัน นี่คือตัวอย่างเล็กน้อย:

```Swift
1> let greeting = "Hello, REPL!"
greeting: String = "Hello, REPL!"
2> print(greeting)
Hello, REPL!
```

ออกด้วย `:quit` หรือ `Control-D`.

## การดำดิ่งลึก
รากฐานของ REPL ย้อนกลับไปยังตัวแปลภาษา Lisp ในยุค 60's Swift’s REPL ตั้งอยู่บน LLVM, กรอบการทำงานของคอมไพเลอร์ที่ทรงพลัง, มอบมากกว่าแค่การตีความพื้นฐาน - มันเป็นเครื่องมือที่เต็มรูปแบบพร้อมฟีเจอร์การเติมข้อความอัตโนมัติ, การดีบัก, และอื่นๆ อีกมากมาย การใช้ REPL นั้นเหมาะสำหรับการเรียนรู้หรือการสร้างต้นแบบ แต่มันไม่ใช่สภาพแวดล้อมการพัฒนาอย่างอิสระ บางคนชอบใช้ Playgrounds ใน Xcode เพื่อการเข้าถึงที่เป็นกราฟิกและอิงตามไฟล์มากขึ้น ในขณะที่คนอื่นๆ ยังคงใช้การแก้ไขสคริปต์และการรันแบบดั้งเดิม

ภายในเบื้องลึก, การทำงานของ Swift's REPL คือการคอมไพล์โค้ดไปยังภาษาเครื่องและทำการรัน ซึ่งเป็นเหตุผลว่าทำไมมันถึงค่อนข้างเร็ว REPL ยังสามารถเข้าถึงโมดูล Swift ที่คอมไพล์ไว้ได้, หรือแม้กระทั่งไลบรารี C, ทำให้มันทรงพลังอย่างมาก อย่างไรก็ตาม, ให้ทราบว่าไม่ใช่ทุกอย่างทำงานได้ดีใน REPL; คุณสมบัติบางอย่างของ Swift โดยเฉพาะที่ต้องการการตั้งค่าโปรเจ็กต์ที่ซับซ้อนหรือไฟล์ storyboard อาจไม่สามารถใช้งานได้ที่นี่

## ดูเพิ่มเติม
- [Swift.org - การเริ่มต้นใช้งาน](https://www.swift.org/getting-started/#using-the-repl)
- [แนะนำการใช้งาน Xcode Playgrounds ของ Apple](https://developer.apple.com/videos/play/wwdc2014/408/)
- [โครงการ LLVM](https://llvm.org/)

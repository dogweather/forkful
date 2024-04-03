---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:33.053011-06:00
description: "REPL (Read-Eval-Print Loop) \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E20\u0E32\
  \u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\
  \u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E04\u0E2D\u0E21\u0E1E\u0E34\
  \u0E27\u0E40\u0E15\u0E2D\u0E23\u0E4C\u0E17\u0E35\u0E48\u0E07\u0E48\u0E32\u0E22\u0E41\
  \u0E25\u0E30\u0E21\u0E35\u0E1B\u0E0F\u0E34\u0E2A\u0E31\u0E21\u0E1E\u0E31\u0E19\u0E18\
  \u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\
  \u0E0A\u0E49\u0E21\u0E31\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\
  \u0E17\u0E14\u0E25\u0E2D\u0E07\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E04\u0E49\u0E14\
  \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27,\u2026"
lastmod: '2024-03-17T21:57:56.177036-06:00'
model: gpt-4-0125-preview
summary: "REPL (Read-Eval-Print Loop) \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E20\u0E32\u0E1E\
  \u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\
  \u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E04\u0E2D\u0E21\u0E1E\u0E34\u0E27\
  \u0E40\u0E15\u0E2D\u0E23\u0E4C\u0E17\u0E35\u0E48\u0E07\u0E48\u0E32\u0E22\u0E41\u0E25\
  \u0E30\u0E21\u0E35\u0E1B\u0E0F\u0E34\u0E2A\u0E31\u0E21\u0E1E\u0E31\u0E19\u0E18\u0E4C\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\
  \u0E0A\u0E49\u0E21\u0E31\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\
  \u0E17\u0E14\u0E25\u0E2D\u0E07\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E04\u0E49\u0E14\
  \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27, \u0E17\
  \u0E14\u0E2A\u0E2D\u0E1A\u0E2A\u0E48\u0E27\u0E19\u0E22\u0E48\u0E2D\u0E22\u0E02\u0E2D\
  \u0E07\u0E42\u0E04\u0E49\u0E14, \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E23\u0E35\u0E22\
  \u0E19\u0E23\u0E39\u0E49\u0E44\u0E27\u0E22\u0E32\u0E01\u0E23\u0E13\u0E4C\u0E02\u0E2D\
  \u0E07\u0E20\u0E32\u0E29\u0E32\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\
  \u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\
  \u0E0A\u0E31\u0E19\u0E40\u0E15\u0E47\u0E21\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
weight: 34
---

## อะไร & ทำไม?
REPL (Read-Eval-Print Loop) เป็นสภาพแวดล้อมการเขียนโปรแกรมคอมพิวเตอร์ที่ง่ายและมีปฏิสัมพันธ์ โปรแกรมเมอร์ใช้มันสำหรับการทดลองเขียนโค้ดอย่างรวดเร็ว, ทดสอบส่วนย่อยของโค้ด, หรือเรียนรู้ไวยากรณ์ของภาษาโดยไม่ต้องสร้างแอปพลิเคชันเต็มรูปแบบ

## วิธีการ:
การเปิดใช้งาน REPL ของ Kotlin นั้นง่ายมาก เปิดเทอร์มินัลของคุณและพิมพ์ `kotlinc` คุณจะเข้าสู่ shell ของ Kotlin ลองกำหนดตัวแปรและพิมพ์ค่าของมันดู:

```kotlin
ยินดีต้อนรับสู่ Kotlin version 1.7.10 (JRE 1.8.0_292-b10)
พิมพ์ :help สำหรับความช่วยเหลือ, :quit สำหรับออก
>>> val greeting = "Hello, Kotlin REPL!"
>>> println(greeting)
Hello, Kotlin REPL!
```

## Deep Dive
REPL ของ Kotlin เปิดตัวพร้อมกับภาษาเพื่อส่งเสริมการทดลอง มีความคล้ายคลึงกับ shell ที่มีปฏิสัมพันธ์ของ Python แต่ถูกปรับให้เหมาะกับไวยากรณ์และความเฉพาะเจาะจงของ Kotlin ทางเลือกอื่น? สภาพแวดล้อมที่มีปฏิสัมพันธ์ใน IDE ต่างๆ เช่น IntelliJ IDEA และสนามเด็กเล่นออนไลน์ของ Kotlin REPL ทำงานโดยการคอมไพล์โค้ดในขณะนั้นเลย ให้ข้อเสนอแนะทันที - สิ่งสำคัญสำหรับการเรียนรู้และการแก้ไขข้อผิดพลาด

## ดูเพิ่มเติม
- เอกสารข้อมูลของ Kotlin เกี่ยวกับ REPL: [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- ทดลองใช้ Kotlin ในเบราว์เซอร์: [https://play.kotlinlang.org](https://play.kotlinlang.org)
- ปลั๊กอิน Kotlin Playground สำหรับ IntelliJ IDEA จาก JetBrains.

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:32.843586-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Kotlin, \u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E43\u0E2B\
  \u0E49\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\
  \u0E2B\u0E0D\u0E48\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E1F\u0E31\u0E07\u0E01\u0E4C\
  \u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\
  \u0E31\u0E1A\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E48\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E43\
  \u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E48\u0E08\u0E32\u0E01\u0E20\
  \u0E32\u0E22\u0E19\u0E2D\u0E01 \u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\u0E01\
  \ Kotlin\u2026"
lastmod: '2024-03-17T21:57:56.156846-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Kotlin, \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\
  \u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E42\u0E14\u0E22\u0E43\u0E0A\
  \u0E49\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E21\u0E32\
  \u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\
  \u0E35\u0E48\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E42\u0E14\u0E22\u0E44\u0E21\
  \u0E48\u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\
  \u0E35\u0E48\u0E08\u0E32\u0E01\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01 \u0E40\u0E19\u0E37\
  \u0E48\u0E2D\u0E07\u0E08\u0E32\u0E01 Kotlin \u0E21\u0E35\u0E27\u0E34\u0E18\u0E35\
  \u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\
  \u0E32\u0E41\u0E25\u0E30\u0E01\u0E23\u0E30\u0E0A\u0E31\u0E1A\n\n#."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E15\u0E31\u0E27\u0E2D\u0E31\
  \u0E01\u0E29\u0E23\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 2
---

## วิธีการ:
ใน Kotlin, สตริงสามารถทำให้เป็นตัวพิมพ์ใหญ่โดยใช้ฟังก์ชันที่มาพร้อมกับไลบรารี่มาตรฐานโดยไม่ต้องใช้ไลบรารี่จากภายนอก เนื่องจาก Kotlin มีวิธีการจัดการสตริงอย่างตรงไปตรงมาและกระชับ

### ทำให้ทั้งสติงเป็นตัวพิมพ์ใหญ่:
```kotlin
val message = "hello, world!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // ผลลัพธ์: HELLO, WORLD!
```

### ทำให้เฉพาะตัวอักษรตัวแรกเป็นตัวพิมพ์ใหญ่:
นับตั้งแต่ Kotlin 1.5, ฟังก์ชัน `capitalize()` ได้ถูกลบออกและแทนที่ด้วยการรวมกันของ `replaceFirstChar` กับ lambda ที่ตรวจสอบว่าเป็นตัวอักษรพิมพ์เล็กเพื่อเปลี่ยนเป็นตัวพิมพ์ใหญ่

```kotlin
val greeting = "hello, world!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // ผลลัพธ์: Hello, world!
```

วิธีการนี้รักษาส่วนที่เหลือของประโยคให้อยู่ในรูปแบบเดิมขณะที่เปลี่ยนเพียงตัวอักษรตัวแรกให้เป็นตัวพิมพ์ใหญ่

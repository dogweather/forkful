---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:48.027183-06:00
description: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19 Regular Expressions\
  \ (regex) \u0E40\u0E1B\u0E47\u0E19\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\
  \u0E37\u0E2D\u0E17\u0E23\u0E07\u0E1E\u0E25\u0E31\u0E07\u0E2A\u0E33\u0E2B\u0E23\u0E31\
  \u0E1A\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21, \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E04\u0E49\u0E19\u0E2B\u0E32, \u0E08\u0E31\u0E1A\u0E04\u0E39\u0E48,\u2026"
lastmod: '2024-03-17T21:57:56.164067-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19 Regular Expressions\
  \ (regex) \u0E40\u0E1B\u0E47\u0E19\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\
  \u0E37\u0E2D\u0E17\u0E23\u0E07\u0E1E\u0E25\u0E31\u0E07\u0E2A\u0E33\u0E2B\u0E23\u0E31\
  \u0E1A\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21, \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E04\u0E49\u0E19\u0E2B\u0E32, \u0E08\u0E31\u0E1A\u0E04\u0E39\u0E48,\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E40\u0E23\u0E01\u0E38\u0E25\u0E32\u0E23\
  \u0E4C\u0E40\u0E2D\u0E47\u0E01\u0E40\u0E1E\u0E23\u0E2A\u0E0A\u0E31\u0E19"
weight: 11
---

## อะไร & ทำไม?

การใช้งาน Regular Expressions (regex) เป็นเครื่องมือทรงพลังสำหรับการประมวลผลข้อความ, ทำให้โปรแกรมเมอร์สามารถค้นหา, จับคู่, และจัดการสตริงด้วยเทคนิคการจับคู่รูปแบบขั้นสูงได้ Kotlin ช่วยให้สามารถทำงานประมวลผลข้อความที่ซับซ้อนได้อย่างมีประสิทธิภาพ เช่น การตรวจสอบความถูกต้อง, การแยกวิเคราะห์, หรือการแปลงข้อความ, ทำให้เป็นเครื่องมือที่ห้ามพลาดสำหรับงานตั้งแต่การจัดการสตริงง่ายๆ ไปจนถึงการวิเคราะห์ข้อความที่ซับซ้อน

## วิธีการ:

### การจับคู่พื้นฐาน
เพื่อตรวจสอบว่าสตริงตรงกับรูปแบบใดรูปแบบหนึ่งใน Kotlin, คุณสามารถใช้เมธอด `matches` ของคลาส `Regex` ได้

```kotlin
val pattern = "kotlin".toRegex()
val input = "I love kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // ผลลัพธ์: true
```

### การค้นหาและแยกส่วนของสตริง
หากคุณต้องการค้นหาส่วนของสตริงที่ตรงกับรูปแบบใดรูปแบบหนึ่ง, Kotlin ช่วยให้คุณสามารถทำการวนซ้ำผ่านการจับคู่ทั้งหมดได้:

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "วันนี้คือวันที่ 07/09/2023."
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// ผลลัพธ์: 07/09/2023
```

### การแทนที่ข้อความ
การแทนที่ส่วนของสตริงที่ตรงกับรูปแบบหนึ่งเป็นเรื่องง่ายด้วยฟังก์ชั่น `replace`:

```kotlin
val input = "Username: user123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // ผลลัพธ์: Username: userXXX
```

### การแบ่งสตริง
แบ่งสตริงออกเป็นรายการ, โดยใช้รูปแบบ regex ในฐานะตัวแบ่ง:

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // ผลลัพธ์: [1, 2, 3, 4, 5]
```

### ไลบรารีอื่น ๆ: Kotest
[Kotest](https://github.com/kotest/kotest) เป็นไลบรารีทดสอบในภาษา Kotlin ที่ได้รับความนิยมซึ่งขยายการสนับสนุน regex ที่มีอยู่เดิมของ Kotlin, มีประโยชน์โดยเฉพาะสำหรับการตรวจสอบความถูกต้องในกรณีทดสอบ

```kotlin
// สมมติว่า Kotest ถูกเพิ่มเข้าในโปรเจ็กต์ของคุณ
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@test.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// การทดสอบนี้จะผ่านหากข้อมูลนำเข้าตรงกับรูปแบบอีเมล
```

ด้วยการรวมการใช้งาน regular expressions เข้ากับแอปพลิเคชัน Kotlin ของคุณ, คุณสามารถประมวลผลข้อความได้อย่างมีประสิทธิภาพ ไม่ว่าคุณจะต้องการตรวจสอบข้อมูลนำเข้าจากผู้ใช้, แยกข้อมูลออกมา, หรือแปลงสตริง, รูปแบบ regex มอบโซลูชันที่แข็งแกร่ง

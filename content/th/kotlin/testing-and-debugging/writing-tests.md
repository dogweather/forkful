---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:02.469926-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E43\u0E19 Kotlin \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E19\u0E34\u0E1B\u0E40\u0E1E\u0E15\u0E42\
  \u0E04\u0E49\u0E14\u0E17\u0E35\u0E48\u0E17\u0E33\u0E07\u0E32\u0E19\u0E42\u0E14\u0E22\
  \u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E16\u0E39\u0E01\
  \u0E15\u0E49\u0E2D\u0E07\u0E02\u0E2D\u0E07\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\
  \u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E02\u0E2D\u0E07\u0E42\u0E21\
  \u0E14\u0E39\u0E25\u0E0B\u0E2D\u0E1F\u0E17\u0E4C\u0E41\u0E27\u0E23\u0E4C\u0E02\u0E2D\
  \u0E07\u0E04\u0E38\u0E13\u2026"
lastmod: '2024-03-17T21:57:56.179638-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E43\u0E19 Kotlin \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E19\u0E34\u0E1B\u0E40\u0E1E\u0E15\u0E42\
  \u0E04\u0E49\u0E14\u0E17\u0E35\u0E48\u0E17\u0E33\u0E07\u0E32\u0E19\u0E42\u0E14\u0E22\
  \u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E16\u0E39\u0E01\
  \u0E15\u0E49\u0E2D\u0E07\u0E02\u0E2D\u0E07\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\
  \u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E02\u0E2D\u0E07\u0E42\u0E21\
  \u0E14\u0E39\u0E25\u0E0B\u0E2D\u0E1F\u0E17\u0E4C\u0E41\u0E27\u0E23\u0E4C\u0E02\u0E2D\
  \u0E07\u0E04\u0E38\u0E13\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E01\u0E32\u0E23\u0E17\u0E14\
  \u0E2A\u0E2D\u0E1A"
weight: 36
---

## อะไร & ทำไม?

การเขียนทดสอบใน Kotlin หมายถึงการสร้างสนิปเพตโค้ดที่ทำงานโดยอัตโนมัติเพื่อตรวจสอบความถูกต้องของฟังก์ชันการทำงานของโมดูลซอฟท์แวร์ของคุณ ให้แน่ใจว่าพวกมันทำงานตามที่คาดหวัง นักโปรแกรมเขียนทำเพื่อจับบักในระยะแรก ช่วยในการปรับโครงสร้างโค้ด และให้เอกสารความช่วยเหลือเกี่ยวกับวิธีการที่ส่วนประกอบซอฟต์แวร์ควรจะทำงาน

## วิธีทำ:

Kotlin รองรับการพัฒนาโดยขับเคลื่อนด้วยการทดสอบ ด้วยเฟรมเวิร์กระบบต่างๆ ที่ได้รับความนิยม ได้แก่ JUnit, Kotest และ MockK สำหรับการทำ mocking นี่คือตัวอย่างง่ายๆ โดยใช้ JUnit:

```kotlin
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class CalculatorTest {

    @Test
    fun `adds two numbers`() {
        val calculator = Calculator()
        val result = calculator.add(2, 3)
        assertEquals(5, result)
    }
}

class Calculator {
    fun add(a: Int, b: Int): Int = a + b
}
```

**ผลลัพธ์ตัวอย่าง**

```text
ทดสอบผ่าน
```

สำหรับวิธีการทดสอบที่มีความซับซ้อนมากขึ้น โดยใช้ Kotest ซึ่งเสนอสไตล์การเขียนทดสอบภาษา Kotlin ที่เหมาะสมยิ่งขึ้น ดูตัวอย่างด้านล่าง:

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class CalculatorSpec : StringSpec({
    "การบวก 2 และ 3 ควรได้ผลลัพธ์ 5" {
        val calculator = Calculator()
        calculator.add(2, 3) shouldBe 5
    }
})
```

การใช้ MockK สำหรับการทดสอบด้วย mocks:

```kotlin
import io.mockk.every
import io.mockk.mockk
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ServiceTest {

    private val repository = mockk<Repository>()
    private val service = Service(repository)

    @Test
    fun `การเรียกข้อมูลจะได้ข้อมูลถูกล้อเลียน`() {
        every { repository.getData() } returns "ข้อมูลถูกล้อเลียน"

        val result = service.getData()

        assertEquals("ข้อมูลถูกล้อเลียน", result)
    }
}

class Service(private val repository: Repository) {
    fun getData(): String = repository.getData()
}

interface Repository {
    fun getData(): String
}
```

**ผลลัพธ์ตัวอย่าง**

```text
ทดสอบผ่าน
```

ตัวอย่างเหล่านี้แสดงถึงพื้นฐานในการเขียนการทดสอบยูนิตใน Kotlin ตามที่แอปพลิเคชันของคุณเติบโต ควรพิจารณาสำรวจเทคนิคการทดสอบและเครื่องมือชั้นสูงมากขึ้นที่แต่ละเฟรมเวิร์กให้มา

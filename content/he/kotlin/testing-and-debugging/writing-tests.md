---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:33.855934-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Kotlin \u05EA\u05D5\
  \u05DE\u05DA \u05D1\u05E4\u05D9\u05EA\u05D5\u05D7 \u05DE\u05D5\u05E0\u05D7\u05D4\
  \ \u05D8\u05E1\u05D8\u05D9\u05DD \u05E2\u05DD \u05DE\u05E1\u05D2\u05E8\u05D5\u05EA\
  \ \u05E9\u05D5\u05E0\u05D5\u05EA, \u05D4\u05E4\u05D5\u05E4\u05D5\u05DC\u05E8\u05D9\
  \u05D5\u05EA \u05D1\u05D9\u05D5\u05EA\u05E8 \u05D4\u05DF JUnit, Kotest, \u05D5-MockK\
  \ \u05DC\u05DE\u05D5\u05E7\u05D9\u05DD. \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\
  \u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA\
  \ JUnit."
lastmod: '2024-03-13T22:44:39.276985-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u05EA\u05D5\u05DE\u05DA \u05D1\u05E4\u05D9\u05EA\u05D5\u05D7 \u05DE\
  \u05D5\u05E0\u05D7\u05D4 \u05D8\u05E1\u05D8\u05D9\u05DD \u05E2\u05DD \u05DE\u05E1\
  \u05D2\u05E8\u05D5\u05EA \u05E9\u05D5\u05E0\u05D5\u05EA, \u05D4\u05E4\u05D5\u05E4\
  \u05D5\u05DC\u05E8\u05D9\u05D5\u05EA \u05D1\u05D9\u05D5\u05EA\u05E8 \u05D4\u05DF\
  \ JUnit, Kotest, \u05D5-MockK \u05DC\u05DE\u05D5\u05E7\u05D9\u05DD."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
weight: 36
---

## איך לעשות:
Kotlin תומך בפיתוח מונחה טסטים עם מסגרות שונות, הפופולריות ביותר הן JUnit, Kotest, ו-MockK למוקים. הנה דוגמה פשוטה באמצעות JUnit:

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

**פלט לדוגמה**

```text
המבחן עבר.
```

לגישה מתקדמת יותר לטסטים באמצעות Kotest, שמציע סגנון כתיבת טסטים יותר אידיומטי ב-Kotlin, ראו את הדוגמה למטה:

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class CalculatorSpec : StringSpec({
    "adding 2 and 3 should return 5" {
        val calculator = Calculator()
        calculator.add(2, 3) shouldBe 5
    }
})
```

השימוש ב-MockK לבדיקות עם מוקים:

```kotlin
import io.mockk.every
import io.mockk.mockk
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ServiceTest {

    private val repository = mockk<Repository>()
    private val service = Service(repository)

    @Test
    fun `get data returns mocked data`() {
        every { repository.getData() } returns "Mocked Data"

        val result = service.getData()

        assertEquals("Mocked Data", result)
    }
}

class Service(private val repository: Repository) {
    fun getData(): String = repository.getData()
}

interface Repository {
    fun getData(): String
}
```

**פלט לדוגמה**

```text
המבחן עבר.
```

הדוגמאות הללו ממחישות את היסודות של כתיבת מבחני יחידה ב-Kotlin. ככל שהאפליקציה שלך גדלה, שקול לחקור טכניקות וכלים מתקדמים יותר לבדיקות, שמסופקים על ידי כל מסגרת.

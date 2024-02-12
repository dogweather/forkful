---
title:                "כתיבת בדיקות"
aliases: - /he/kotlin/writing-tests.md
date:                  2024-02-03T19:31:33.855934-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת בדיקות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת מבחנים ב-Kotlin כוללת יצירת קטעי קוד שמאמתים באופן אוטומטי את נכונות התפקוד של מודולי התוכנה שלך, ומבטיחים שהם פועלים כצפוי. מתכנתים עושים את זה כדי לתפוס תקלות בשלב מוקדם, להקל על שיפוט קוד, ולספק תיעוד על איך מרכיבי התוכנה אמורים לפעול.

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

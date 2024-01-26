---
title:                "כתיבת בדיקות"
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות זה פרקטיקה שבה תוכניתנים יוצרים טסטים לקוד שלהם כדי לוודא שהכל עובד כשורה. זה נעשה כדי למנוע באגים, לשפר את איכות הקוד, ולהקל על שינויים בעתיד.

## איך לעשות:
```Kotlin
import org.junit.Test
import org.junit.Assert.*

class ExampleUnitTest {
    @Test
    fun addition_isCorrect() {
        assertEquals(4, 2 + 2)
    }
}
```
פלט לדוגמה:
```
Test passed: addition_isCorrect
```

## צלילה עמוקה:
כבר מהשנים הראשונות של תכנות, מובן הייתה החשיבות של בדיקות קוד. יש כלים וספריות רבות לבדיקות - JUnit הוא רק דוגמה אחת. מימוש הבדיקות עשוי להשתנות, אך העיקרון נשאר: ודא שהתוכנה עובדת כשורה.

## ראו גם:
- [JUnit 5 User Guide](https://junit.org/junit5/docs/current/user-guide/)
- [Test-Driven Development (TDD)](https://en.wikipedia.org/wiki/Test-driven_development)

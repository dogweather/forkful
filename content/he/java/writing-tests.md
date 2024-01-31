---
title:                "כתיבת בדיקות"
date:                  2024-01-19
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
כתיבת בדיקות היא בניית קוד שמוודא את תקינות קוד אחר. תכנתים כדי לשפר את האיכות, לזהות באגים ולהקל על תחזוקה.

## How to: (איך לעשות:)
בואו נראה איך כותבים בדיקה פשוטה עם JUnit. 

```java
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

class CalculatorTest {

    @Test
    void testAddition() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 should equal 5");
    }
}

class Calculator {
    int add(int a, int b) {
        return a + b;
    }
}
```
פלט לדוגמא אם הבדיקה עוברת:
```
Test passed.
```

או פלט לדוגמא אם היא נכשלת:
```
org.junit.ComparisonFailure: 2 + 3 should equal 5 expected:<5> but was:<4>
```

## Deep Dive (צלילה עמוקה)
JUnit התחיל כפרויקט ב-1997. הוא הופך לסטנדרט לכתיבת בדיקות ב-Java. ישנם אלטרנטיבות כמו TestNG או Spock. כתיבת בדיקות דורשת שימוש במערכת בניית תוכנה כמו Maven או Gradle ולעיתים הטמעה עם CI/CD.

## See Also (ראה גם)
- [JUnit 5 User Guide](https://junit.org/junit5/docs/current/user-guide/)
- [Introduction to CI/CD with GitLab](https://docs.gitlab.com/ee/ci/introduction/)

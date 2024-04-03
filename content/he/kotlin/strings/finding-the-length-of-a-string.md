---
date: 2024-01-20 17:47:54.583540-07:00
description: "How to: (\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA:) \u05E9\u05D0\
  \u05D9\u05DC\u05EA\u05EA \u05D4\u05D0\u05D5\u05E8\u05DA \u05E9\u05DC \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D1\u05E7\u05D5\u05D8\u05DC\u05D9\u05DF \u05D4\u05D9\
  \u05D0 \u05E6'\u05D9\u05E7 \u05E6'\u05E7."
lastmod: '2024-03-13T22:44:39.256734-06:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05D0\u05D9\u05DC\u05EA\u05EA \u05D4\u05D0\u05D5\u05E8\u05DA \u05E9\
  \u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\u05E7\u05D5\u05D8\u05DC\u05D9\
  \u05DF \u05D4\u05D9\u05D0 \u05E6'\u05D9\u05E7 \u05E6'\u05E7."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
weight: 7
---

## How to: (איך לעשות:)
שאילתת האורך של מחרוזת בקוטלין היא צ'יק צ'ק:

```Kotlin
fun main() {
    val greeting = "שלום לכולם!"
    println("אורך המחרוזת: ${greeting.length}")
}
```

תוצאת הדוגמה:
```
אורך המחרוזת: 12
```

## Deep Dive (צלילה עמוקה)
הפונקציה `.length` בקוטלין יורשת מ-Java, הידועה ביציבות ונסיון. אופציות אלטרנטיביות? יש את `.count()`, אבל `.length` יותר ישיר ומהיר כאשר רק אורך המחרוזת נדרש. 
בפנים, `.length` היא פרופרטי שמחזירה את גודל המערך שמייצג את המחרוזת.

## See Also (ראה גם)
- [אורך מחרוזת ב- Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html)
- [המבנה הפנימי של מחרוזות ב-Java/Kotlin](https://www.baeldung.com/java-string-pool)

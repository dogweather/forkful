---
title:                "מציאת אורך מחרוזת"
aliases:
- /he/kotlin/finding-the-length-of-a-string/
date:                  2024-01-20T17:47:54.583540-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
כשאמרים "אורך של מחרוזת", מתכוונים למספר התווים בה. מתכנתים צריכים לדעת את זה לבדיקות, תיקוני מחרוזות, ועוד.

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

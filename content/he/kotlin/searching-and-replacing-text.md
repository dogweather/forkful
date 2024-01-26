---
title:                "חיפוש והחלפת טקסט"
date:                  2024-01-20T17:58:48.223945-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
חיפוש והחלפת טקסט מאפשר לנו למצוא מחרוזות בתוך טקסט ולהחליפן במחרוזות אחרות. מתכנתים עושים זאת לתיקון, עדכון או פורמט של נתונים.

## How to: (איך לעשות:)
```kotlin
// חיפוש מילים במחרוזת והחלפתן
val originalText = "שלום עולם! כתיבת קוד זה כיף."
val newText = originalText.replace("כתיבת קוד", "לתכנת")
println(newText) // תוצאה: שלום עולם! לתכנת זה כיף.
```

```kotlin
// חיפוש והחלפה עם ביטויים רגולריים
val regexText = "אני כותב בKotlin בשנת 2023"
val regex = "\\d{4}".toRegex() // מחפש מספר בן ארבע ספרות
val updatedText = regexText.replaceFirst(regex, "2024")
println(updatedText) // תוצאה: אני כותב בKotlin בשנת 2024
```

## Deep Dive (לעומק הנושא)
חיפוש והחלפת טקסט הם בסיס לעיבוד טקסט ומגיעים מימי המחשבים הראשונים. באופן היסטורי, עורכי טקסט כמו vi וsed היו חלוצים בתחום. בקוטלין, יש גישה פשוטה לחיפוש והחלפה, אבל גם אפשרויות מתקדמות עם ביטויים רגולריים - Regular Expressions.

הרחבה יכולה להיות עם מטודות כמו `replaceBefore` ו`replaceAfter`. קוטלין גם מאפשר לכם להגדיר את סוג ההחלפה – רק ההתאמה הראשונה או כל ההתאמות.

אלטרנטיבות כוללות שימוש במחלקה `StringBuilder` לעבודה יעילה עם מחרוזות גדולות, או ביבליות חיצוניות עבור פונקציונליות מיוחדת יותר.

## See Also (ראה גם)
- [Kotlin Text](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/) - תיעוד הAPI הרשמי לעבודה עם טקסט בקוטלין.
- [Regular Expressions in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/) - גישה לעבודה עם Regular Expressions בקוטלין.
- [Kotlin Playground](https://play.kotlinlang.org/) - סביבת פיתוח אינטראקטיבית לניסוי בקוטלין בדפדפן.

---
date: 2024-01-20 17:58:48.223945-07:00
description: "How to: (\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA:) \u05D7\u05D9\
  \u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\u05E7\u05E1\u05D8\
  \ \u05D4\u05DD \u05D1\u05E1\u05D9\u05E1 \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3 \u05D8\
  \u05E7\u05E1\u05D8 \u05D5\u05DE\u05D2\u05D9\u05E2\u05D9\u05DD \u05DE\u05D9\u05DE\
  \u05D9 \u05D4\u05DE\u05D7\u05E9\u05D1\u05D9\u05DD \u05D4\u05E8\u05D0\u05E9\u05D5\
  \u05E0\u05D9\u05DD. \u05D1\u05D0\u05D5\u05E4\u05DF \u05D4\u05D9\u05E1\u05D8\u05D5\
  \u05E8\u05D9, \u05E2\u05D5\u05E8\u05DB\u05D9 \u05D8\u05E7\u05E1\u05D8 \u05DB\u05DE\
  \u05D5 vi \u05D5sed \u05D4\u05D9\u05D5 \u05D7\u05DC\u05D5\u05E6\u05D9\u05DD \u05D1\
  \u05EA\u05D7\u05D5\u05DD. \u05D1\u05E7\u05D5\u05D8\u05DC\u05D9\u05DF,\u2026"
lastmod: '2024-04-05T22:50:53.446674-06:00'
model: gpt-4-1106-preview
summary: "(\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA:) \u05D7\u05D9\u05E4\u05D5\
  \u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\u05E7\u05E1\u05D8 \u05D4\u05DD\
  \ \u05D1\u05E1\u05D9\u05E1 \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3 \u05D8\u05E7\u05E1\
  \u05D8 \u05D5\u05DE\u05D2\u05D9\u05E2\u05D9\u05DD \u05DE\u05D9\u05DE\u05D9 \u05D4\
  \u05DE\u05D7\u05E9\u05D1\u05D9\u05DD \u05D4\u05E8\u05D0\u05E9\u05D5\u05E0\u05D9\u05DD\
  ."
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
weight: 10
---

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

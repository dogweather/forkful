---
title:                "שימוש בביטויי רגילים"
html_title:           "Kotlin: שימוש בביטויי רגילים"
simple_title:         "שימוש בביטויי רגילים"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
ממשקי רגילה משמשים לזיהוי תבניות בטקסטים. כאשר מצליחים למצוא תבנית זו בטקסט, ניתן לבצע פעולות מסוימות עליה כמו חילוץ או החלפת טקסט. תכונה זו לא נמצאת רק בשפת קוד קוטלינג, אלא גם במגוון שפות תכנות אחרות, ומשמשת לפתרון תחזיות ולניתוח נתונים טקסטואליים. בגלל כמויות הנתונים הגדלות היום, ממשקי רגילה מכפילים את כמויות העבודה שפתרו בעבר במשך כמה דקות בלבד.

## כיצד לעשות זאת:
```Kotlin
// ייבוא חבילת ממשקים בשביל להשתמש בממשקי רגילה
import kotlin.text.Regex

fun main() {

    // צור תבניות באמצעות אוסף של תווים יחסיים
	val pattern = Regex("[0-9]")
    val pattern2 = Regex("Hello")

    // חפש תבניות חוזרות בטקסט
    val result = pattern.replace("There are 1001 reasons to say Hello!", "X")

    // הדפס הוצאה
    println(result)
    // הוצאה סופית: "There are XXX reasons to say XXX!"
}
```

## כניסה רעועה:
ממשקי רגילה נכתבו בשנות ה-1950 כדי לטפל בתבניות בטקסט והפכו לקידום חשיבה לכל מחשב. כיום, ישנן אפשרויות רבות לניתוח טקסט והשוואה לבין ממשקי רגילה לפתרון בעיות ספציפיות. לאותן מטרות השתמשו בכלים כמו ביטויים רגילים, תיקון הטקסט של אלגוריתמים ותמצית ביטויים.

## ראה גם:
- [מאמר בבלוג הרשמי של קודטלין על ממשקי רגילה](https://blog.jetbrains.com/kotlin/2020/06/kotlin-1-4-gets-regexes/)
- [הכניסה לממשקי הרגילה של קודטינלין בעמוד התיעוד של קודטלין](https://kotlinlang.org/docs/regular-expressions.html)
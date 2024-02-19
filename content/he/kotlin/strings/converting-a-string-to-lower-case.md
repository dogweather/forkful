---
aliases:
- /he/kotlin/converting-a-string-to-lower-case/
date: 2024-01-20 17:40:05.182995-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D6\u05D4\
  \ \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D4\u05DE\u05D9\u05E8 \u05D0\u05EA \u05DB\u05DC\
  \ \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D4\u05D9\u05D5\u05EA \u05D1\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\
  \u05D8\u05E0\u05D5\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\
  \u05D0 \u05D4\u05E9\u05D5\u05D5\u05D0\u05D4 \u05D0\u05D7\u05D9\u05D3\u05D4 \u05D5\
  \u05DC\u05D0 \u05E8\u05D2\u05D9\u05E9\u05D4 \u05DC\u05E8\u05D9\u05E9\u05D9\u05D5\
  \u05EA \u05D1\u05D9\u05DF \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA."
lastmod: 2024-02-18 23:08:52.783326
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D6\u05D4 \u05E4\
  \u05E9\u05D5\u05D8 \u05DC\u05D4\u05DE\u05D9\u05E8 \u05D0\u05EA \u05DB\u05DC \u05D4\
  \u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D4\u05D9\u05D5\u05EA \u05D1\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\
  \u05E0\u05D5\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0\
  \ \u05D4\u05E9\u05D5\u05D5\u05D0\u05D4 \u05D0\u05D7\u05D9\u05D3\u05D4 \u05D5\u05DC\
  \u05D0 \u05E8\u05D2\u05D9\u05E9\u05D4 \u05DC\u05E8\u05D9\u05E9\u05D9\u05D5\u05EA\
  \ \u05D1\u05D9\u05DF \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA."
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות זה פשוט להמיר את כל התווים במחרוזת להיות באותיות קטנות. תכנתים עושים את זה כדי לוודא השוואה אחידה ולא רגישה לרישיות בין מחרוזות.

## איך לעשות:
בקוטלין, זה קל מאוד להמיר מחרוזת לאותיות קטנות:

```Kotlin
fun main() {
    val original = "Shalom Olam!"
    val lowerCased = original.toLowerCase()
    println(lowerCased)
}
```

פלט לדוגמא:

```
shalom olam!
```

## עיון מעמיק
ההמרה לאותיות קטנות היא פונקציה פשוטה שממומשת בשפות תכנות רבות, והיא קיימת כבר עשרות שנים. בקוטלין, `toLowerCase()` מבוססת על תקן Unicode ולכן יעבוד עם מגוון רחב של תווים, לא רק עברית או אנגלית. יש גם `toUpperCase()`, למי שרוצה להמיר לאותיות גדולות. מבחינת פרטי הביצוע, `toLowerCase()` יכולה להשפיע על ביצועי האפליקציה אם משתמשים בה בתדירות גבוהה על מחרוזות גדולות מאוד, אבל לרוב זה לא משהו שצריך לדאוג ממנו.

## ראה גם
- תיעוד המחלקה `String` של קוטלין: [Kotlin Standard Library: String](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- עמוד על אותיות קטנות ב-Unicode: [Unicode Case Folding](https://www.unicode.org/reports/tr21/tr21-5.html)

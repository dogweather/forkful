---
title:                "המרת מחרוזת לאותיות קטנות"
date:                  2024-01-20T17:40:05.182995-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"

category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/converting-a-string-to-lower-case.md"
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

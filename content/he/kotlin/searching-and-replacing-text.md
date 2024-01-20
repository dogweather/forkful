---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפת טקסט הם שני פעולות שממירות מחרוזת אחת לאחרת. תכנתים אותן לתקן טקסט או לשלוט בנתונים.

## איך לעשות זאת:
בכדי לעשות חיפוש והחלפה של מחרוזת בKotlin, אפשר להשתמש בפונקציית replace().
```Kotlin
val text = "זה הוא טקסט מדגם"
val newText = text.replace("מדגם", "חדש")
println(newText) // מודפס: "זה הוא טקסט חדש"
```
## צלילה עמוקה:
בעבר, הפונקציה replace() הייתה ממומשת באמצעות פקודות לולאה טרדיציונאליות, אך Kotlin מבצעת שיפור זה באמצעות Regular Expressions. גרסאות ברנופ"פ חזורה מאפשרות התאמה גדולה יותר. יש גם אפשרויות אחרות לחיפוש והחלפה, כמו הפונקציה split().
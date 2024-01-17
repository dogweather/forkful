---
title:                "יצירת מספרים אקראיים"
html_title:           "Kotlin: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

מה ולמה?

תכנותנים לרוב נתקלים במצבים בהם הם צריכים ליצור מספרים אקראיים. כדי לסייע בפתרון בעיות ובבדיקת קוד, מפתחי תוכניות ניצלים מודלים שנועדו לייצר מספרים אקראיים.

איך לעשות?

```Kotlin
import java.util.Random
// יצירת מופע של גרעין-מספר-אקראי מסוים באמצעות מדוגם
val random = Random()
// יצירת מספר שלם אקראי בתחום שהמשתמש אומר
val randomNumber = random.nextInt(100)
// בדיקת המספר האקראי
println(randomNumber) // יציב על המסך תוצאה כגון 52
```

עיון מעמיק

הגרעין-מספר-אקראי הוא תכונה פונקציונלית חשובה שמשמשת ליצירת מספרים אקראיים. ניתן להשתמש במודלים שונים עבור יצירת מספרים אקראיים כגון מודל הליניארי, מודל המודולרי ועוד. בנוסף, טכנולוגיות נוספות כמו תוכניות מחשב כמו פייתון וגם שפות תכנות כמו לואה משתמשות בתכונה של הגרעין-מספר-אקראי ליצירת מספרים אקראיים.

ראה גם

למידע נוסף על הגרעין-מספר-אקראי בקוד, יש להתייחס ללינקים הבאים:

- https://kotlinlang.org/docs/reference/basic-types.html
- https://docs.oracle.com/javase/10/docs/api/java/util/Random.html
- https://en.wikipedia.org/wiki/Random_number_generation
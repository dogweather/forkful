---
title:                "הגדלת אותיות במחרוזת"
aliases:
- /he/kotlin/capitalizing-a-string.md
date:                  2024-02-03T19:06:05.162111-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

להון אות ראשונה במחרוזת בתכנות כולל המרה של התו הראשון של המחרוזת לאות גדולה, אם הוא כבר לא כזה, מה ששימושי לעיצוב קלטי משתמש או הצגת טקסט בממשק משתמש באופן יותר תקני או ידידותי לאדם. מתכנתים מבצעים את הפעולה הזו כדי להבטיח אחידות נתונים או לעמוד בדרישות עיצוב מסוימות בתוך האפליקציות התוכנה שלהם.

## איך לעשות:

בקוטלין, ניתן להון מחרוזות באמצעות פונקציות הספרייה הסטנדרטית ללא הצורך בספריות צד שלישי. גישת קוטלין לטיפול במחרוזות הופכת את הפעולות האלה לפשוטות ותמציתיות.

### הונה של כל המחרוזת:

```kotlin
val message = "hello, world!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // פלט: HELLO, WORLD!
```

### הונה של התו הראשון בלבד:

נכון לקוטלין 1.5, פונקציית ה`capitalize()` אינה בשימוש והוחלפה בשילוב של `replaceFirstChar` ולמבדא שבודק אם זה אות קטנה כדי להמיר אותה לאות גדולה.

```kotlin
val greeting = "hello, world!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // פלט: Hello, world!
```

הגישה הזו שומרת על שאר המשפט בצורתו המקורית תוך שינוי האות הראשונה בלבד לאות גדולה.

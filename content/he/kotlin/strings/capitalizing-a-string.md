---
aliases:
- /he/kotlin/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:05.162111-07:00
description: "\u05DC\u05D4\u05D5\u05DF \u05D0\u05D5\u05EA \u05E8\u05D0\u05E9\u05D5\
  \u05E0\u05D4 \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\u05EA\u05DB\u05E0\
  \u05D5\u05EA \u05DB\u05D5\u05DC\u05DC \u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05D4\
  \u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\u05DF \u05E9\u05DC \u05D4\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4\
  , \u05D0\u05DD \u05D4\u05D5\u05D0 \u05DB\u05D1\u05E8 \u05DC\u05D0 \u05DB\u05D6\u05D4\
  , \u05DE\u05D4 \u05E9\u05E9\u05D9\u05DE\u05D5\u05E9\u05D9 \u05DC\u05E2\u05D9\u05E6\
  \u05D5\u05D1 \u05E7\u05DC\u05D8\u05D9 \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D0\u05D5\
  \ \u05D4\u05E6\u05D2\u05EA \u05D8\u05E7\u05E1\u05D8 \u05D1\u05DE\u05DE\u05E9\u05E7\
  \ \u05DE\u05E9\u05EA\u05DE\u05E9\u2026"
lastmod: 2024-02-18 23:08:52.779286
model: gpt-4-0125-preview
summary: "\u05DC\u05D4\u05D5\u05DF \u05D0\u05D5\u05EA \u05E8\u05D0\u05E9\u05D5\u05E0\
  \u05D4 \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\u05EA\u05DB\u05E0\u05D5\
  \u05EA \u05DB\u05D5\u05DC\u05DC \u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05D4\u05EA\
  \u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\u05DF \u05E9\u05DC \u05D4\u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4, \u05D0\
  \u05DD \u05D4\u05D5\u05D0 \u05DB\u05D1\u05E8 \u05DC\u05D0 \u05DB\u05D6\u05D4, \u05DE\
  \u05D4 \u05E9\u05E9\u05D9\u05DE\u05D5\u05E9\u05D9 \u05DC\u05E2\u05D9\u05E6\u05D5\
  \u05D1 \u05E7\u05DC\u05D8\u05D9 \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D0\u05D5 \u05D4\
  \u05E6\u05D2\u05EA \u05D8\u05E7\u05E1\u05D8 \u05D1\u05DE\u05DE\u05E9\u05E7 \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u2026"
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
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

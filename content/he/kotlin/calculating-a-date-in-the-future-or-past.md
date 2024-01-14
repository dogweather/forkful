---
title:                "Kotlin: חישוב תאריך בעתיד או בעבר"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

מחשבתם אי פעם כיצד ניתן לחשב תאריך בעתיד או בעבר באמצעות קוד? הייתם יכולים להשתמש בכך כדי לנהל את חייכם בצורה יותר מאורגנת או למתג את אפליקציות ניידות שלכם? במאמר זה נלמד כיצד לבצע חישוב של תאריך בעתיד או בעבר באמצעות שפת התכנות Kotlin.

## כיצד לעשות זאת

כדי לבצע חישוב של תאריך בעתיד או בעבר, ניתן להשתמש בטכניקה המבוססת על תאריך ההיום וביחידות זמן שתרצו להוסיף או להפחית ממנו. למשל, אם תרצו לחשב תאריך של שבוע לפני זמן מסוים, תוכלו להשתמש בפונקציה `LocalDate.now().minusWeeks(1)` בשפת Kotlin.

ניתן להשתמש גם בממשקים נוספים, כגון הפונקציה `plus` או `minus`, כדי להוסיף או להפחית יחידות זמן מתאריך מסוים. כדי לקבל את התאריך המבוקש בפורמט מסוים, ניתן להשתמש בממשק `DateTimeFormatter` ולהגדיר את הפורמט המבוקש כפרמטר.

כדי להבין כיצד להשתמש בכל אחת מהטכניקות האלו בקוד, הנה דוגמאות של קטעי קוד בשפת Kotlin:

```Kotlin
// חישוב תאריך של חודש לפני זמן מסוים
val pastDate = LocalDate.now().minusMonths(1)
println("תאריך החודש שעבר $pastDate")

// חישוב תאריך של שנה לפני זמן מסוים והצגת התאריך בפורמט המבוקש
val pastYear = LocalDate.now().minusYears(1)
val dateFormatter = DateTimeFormatter.ofPattern("d/M/yyyy")
println("תאריך השנה שעבר ${pastYear.format(dateFormatter)}")
```

## חפירה עמוקה

החישוב של תאריך בעתיד או בעבר מבוסס על תאריך היום והשימוש בפונקציות של יחידות ז
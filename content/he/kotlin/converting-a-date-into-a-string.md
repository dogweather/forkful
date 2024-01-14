---
title:                "Kotlin: המרת תאריך למחרוזת"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה
למה להתעסק בהמרת תאריך למחרוזת? בעזרת המרת תאריך למחרוזת ניתן להציג את התאריך בפורמט המתאים לכם, כמו גם לשמור על קוד נקי ומאורגן. 

## איך לבצע
בכדי להמיר תאריך למחרוזת ניתן להשתמש בפונקציות המובנות של קוטלין. לדוגמה, ניתן להשתמש בפונקציה `format` כדי להציג את התאריך בפורמט שתרצו. 

```Kotlin
val currentDate = LocalDate.now()
val formattedDate = currentDate.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
println(formattedDate) // Output: 30/06/2021
```

ניתן גם להשתמש בפונקציה `toString` כדי להמיר את התאריך למחרוזת בפורמט המתאים. 

```Kotlin
val currentDate = LocalDate.now()
val stringDate = currentDate.toString()
println(stringDate) // Output: 2021-06-30
```

בנוסף, ניתן להשתמש בפונקציות מתקדמות יותר כגון `format` כדי להתאים את התאריך לפי הבקשות שלכם.

## חקירה מעמיקה
הפעולה של המרת תאריך למחרוזת היא נפוצה בקודים שלנו. בכדי להתמודד עם תאריכים בקוד ניתן להשתמש בספריות של קוטלין כמו `java.time` ו-`joda.time`. ניתן גם להתאים את התאריך להעדפותיכם באמצעות התאמה של הפורמט והנוסח של התאריך. חשוב לוודא שהשימוש בפונקציות תמיד מועיל ואינו מציג בעיות אבטחה בקוד שלנו.

## ראה גם
- [כיצד להמיר תאריך למחרוזת ב-Java](https://www.baeldung.com/java-date-to-string)
- [המרת תאריך למחרוזת בתכנות בקוטלין](https://www.geeksforgeeks.org/date-to-string-conversion-in-kotlin/)
- [דוגמאות להמרת תאריך למחרוזת בפייתון](https://www.programiz.com/python-programming/datetime/strftime)
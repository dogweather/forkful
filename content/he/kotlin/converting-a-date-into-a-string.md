---
title:                "Kotlin: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

כתיבת קוד בכיף היא כמו להכנס לעולם של מתח המחשבות. ולכן, קבלת תאריך והמרתו למחרוזת הוא עוד דרך להשתמש ביצירתיות ולפתור בעיות טכניות בקוד שלנו.

## איך לעשות זאת

תהליך ההמרה מתחיל על ידי קבלת תאריך מסוים ושימוש במתודת `.toString()` כדי להמיר אותו למחרוזת.

```Kotlin
val date = LocalDate.of(2021, 10, 31)
val dateString = date.toString()
println(dateString) //output: 2021-10-31
```

במקרים רבים, נרצה לקבל את התאריך בפורמט מסוים. לדוגמה, נרצה שהתאריך יודפס בתוך מחרוזת כך שיופיע רק החודש והשנה. כדי לעשות זאת, נשתמש במתודת `.format()` ונספק לה את הפורמט הרצוי.

```Kotlin
val date = LocalDate.of(2021, 10, 31)
val dateString = date.format(DateTimeFormatter.ofPattern("MM/yyyy"))
println(dateString) //output: 10/2021
```

אם נרצה להוסיף גם את היום לתאריך, אפשר להוסיף עוד אופציות בפורמט.

```Kotlin
val date = LocalDate.of(2021, 10, 31)
val dateString = date.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
println(dateString) //output: 31/10/2021
```

זו רק טעימה מהאפשרויות הרבות שניתן לעשות עם המרת תאריך למחרוזת. בכל מקרה, כל שנצטרך לעשות הוא להתאים את הפורמט לפי הצורך שלנו ולהשתמש במתודות המתאימות.

## חפירה עמוקה

המרת תאריך למחרוזת היא פעולה נפוצה בקוד שלנו ועשויה לקורות כמה בעיות אם לא ניזן אותה כראוי. הנה כמה נקודות חשובות לקחת בחשבון:

- כדי להימנע משגיאות בהמרת תאריך, כדאי למצוא פורמט קבוע ולהשתמש בו בזמן כתיבת הקוד. זה יסייע למנוע בלבול ושגיאות
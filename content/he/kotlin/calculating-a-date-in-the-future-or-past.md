---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Kotlin: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

יש כמה סיבות לרצות לחשב תאריך בעתיד או בעבר. למשל, ייתכן שאנחנו רוצים לתכנן אירוע מראש וצריכים לדעת מתי הוא יתקיים, או שאנחנו רוצים לבדוק מתי נסענו בפעם האחרונה לחופשה. המטרה שלנו כתב מאמר היא ללמוד כיצד לחשב תאריכים בעתיד או בעבר בעזרת שפת תכנות קוטלין.

## איך לעשות זאת

אם נרצה לחשב תאריך בעתיד, ניתן להשתמש בפונקציה `plus` כדי להוסיף ימים, שבועות או חודשים לתאריך הנוכחי. לדוגמה, נרצה לתאריך היום נוסיף 30 ימים ונדפיס אותו:

```Kotlin
val date = LocalDate.now()
val futureDate = date.plus(30, ChronoUnit.DAYS)
println(futureDate)
```

התוצאה תהיה: 26/04/2021

ניתן גם לבצע את הפעולה הזו באופן קצר יותר עם פונקציה כמו `plusDays`:

```Kotlin
val futureDate = date.plusDays(30)
```

אם נרצה לחשב תאריך בעבר, ניתן להשתמש בפונקציה `minus` כדי להחסיר ימים, שבועות או חודשים מתאריך הנוכחי. לדוגמה, נרצה לחשב תאריך מלפני שנתיים ונדפיס אותו:

```Kotlin
val date = LocalDate.now()
val pastDate = date.minus(2, ChronoUnit.YEARS)
println(pastDate)
```

התוצאה תהיה: 01/04/2019

אנחנו יכולים גם להשתמש בפונקציה `minusYears` עבור פעולות חיסור ממוקדות לשנים.

כמו כן, ניתן לייצא תאריכים בפורמטים שונים באמצעות `DateTimeFormatter`. למשל, אם נרצה להדפיס את התאריך הנוכחי בפורמט של "יום/חודש/שנה", נוסיף את הפורמט המתאים בתוך `format()` הפונקציה:

```Kotlin
val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
val currentDate = LocalDate.now()
val formattedDate = currentDate.format(formatter)
println(formattedDate)
```

התוצאה
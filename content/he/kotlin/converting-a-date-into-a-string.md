---
title:                "המרת תאריך למחרוזת"
html_title:           "Kotlin: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה: 
שתי משפטים מקסימליים המסבירים למה מישהו ירצה להשתתף בהמרה של תאריך למחרוזת.

## איך לעשות זאת:
כתיבת דוגמאות קוד ותוצאות דוגמא תוך שימוש בחסימות קוד "```Kotlin...```"

כדי להמיר תאריך למחרוזת בקוד קוטלין, ניתן להשתמש בפונקציה `toString()` הקיימת עבור אובייקטי תאריך. לדוגמה:

```kotlin
val currentDate = LocalDate.now()
val stringDate = currentDate.toString()
println(stringDate)

// Output: 2021-05-17
```

ניתן גם להשתמש בפורמט מותאם אישית עבור המחרוזת המציגה את התאריך. לדוגמה, אם נרצה להציג את התאריך בפורמט של "Day/Month/Year", ניתן לכתוב:

```kotlin
val dateFormat = DateTimeFormatter.ofPattern("dd/MM/yyyy")
val currentDate = LocalDate.now()
val formattedDate = currentDate.format(dateFormat)
println(formattedDate)

// Output: 17/05/2021
```

בנוסף, ניתן להשתמש בפונקציה `format()` על אובייקטי `LocalDateTime` עם פורמט מותאם אישית כדי לציג את הזמן והתאריך באופן מלא. לדוגמה:

```kotlin
val dateTimeFormat = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm")
val currentDateTime = LocalDateTime.now()
val formattedDateTime = currentDateTime.format(dateTimeFormat)
println(formattedDateTime)

// Output: 17/05/2021 12:45
```



## עיון מעמיק:
ישנם כמה דברים שחשוב לקחת בחשבון כאשר ממירים תאריך למחרוזת בקוד קוטלין. ראשית, ישנם פורמטים קבועים שקיימים כגון ISO-8601 המוסכמים לייצג תאריכים וזמנים באופן מקובל בין שפות תכנות שונות. יש גם פורמטים מותאמים אישית למשתמש מתוך צורך להציג תאריכים בצורה מיוחדת.

בנוסף, כדי לשמור על תאריך מדויק ולמנוע שגיאות, חשוב לוודא שהשימוש בפונקציה `toString()` או `format()` מתבצע על אובייקט מסוג `LocalDate` או `LocalDateTime
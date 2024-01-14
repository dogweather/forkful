---
title:                "Kotlin: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

מחישוב תאריך בעתיד או בעבר יכול להיות מועיל עבור תכניות, אפליקציות ואתרים שמתעסקים עם זמינות מוצרים או אירועים בתאריכים מקובלים, כמו חגים, מועדי לידה ועוד. בכתבה זו נלמד כיצד לכתוב קוד Kotlin שיאפשר לנו לחשב תאריך בעתיד או בעבר ולהציג את התוצאות למשתמש.

## איך לעשות את זה

נתחיל עם דוגמה פשוטה של חישוב תאריך בעתיד בקוד Kotlin:

```Kotlin
// הגדרת תאריך התחלה וכמות הימים למחשב
val startDate = LocalDate.now()
val numOfDays = 30 

// חישוב התאריך המבוקש בעתיד
val endDate = startDate.plusDays(numOfDays)

// הודפסת התאריך המבוקש למשתמש
println("התאריך המבוקש הוא: $endDate")
```
הפלט של הקוד הנ"ל יהיה:

```
התאריך המבוקש הוא: 2020-05-13
```

כעת, נרחיב את הקוד כך שיאפשר למשתמש להזין תאריך התחלה ומספר ימים בשביל לחשב תאריך בעתיד:

```Kotlin
// קליטת תאריך התחלה מהמשתמש
println("אנא הכנס תאריך התחלה בפורמט dd/MM/yyyy:")
val input = readLine()!!

// המרת תאריך המחרוזת לטיפוס LocalDate
val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
val startDate = LocalDate.parse(input, formatter)

// קליטת המספר המייצג את מספר הימים
println("אנא הכנס מספר ימים:")
val numOfDays = readLine()!!.toInt()

// חישוב התאריך המבוקש בעתיד
val endDate = startDate.plusDays(numOfDays)

// הודפסת התאריך המבוקש למשתמש
println("התאריך המבוקש הוא: $endDate")
```

כעת, אם נרצה לחשב תאריך בעבר, נוכל להשתמש בפונקציות `minusDays()` ו- `minusMonths()` במקום `plusDays()` ו- `plusMonths()`.

## כיוון עמוק

כדי לחשב תאריך בעתיד או בעבר, השתמשנו בטיפוס LocalDate הכולל מתודות ש
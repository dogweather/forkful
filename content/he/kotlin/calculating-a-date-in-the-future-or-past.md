---
title:    "Kotlin: חישוב תאריך בעתיד או בעבר"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

חישוב תאריך בעתיד או בעבר יכול להיות מועיל במגוון רחב של תחומים, כגון תכנות אירועים או התזמנות מראש.

## כיצד לבצע

כדי לחשב תאריך בעתיד או בעבר ב-Kotlin, ניתן להשתמש במספר מתודות שקיימות בשפה זו.

לדוגמה, נוכל להשתמש במתודה `plus` כדי להוסיף ימים לתאריך קיים:

```Kotlin
val today = LocalDate.now()
val futureDate = today.plus(5, ChronoUnit.DAYS)
```

בדוגמה זו, אנחנו משתמשים במתודה `now` כדי ליצור תאריך נוכחי ואז מקסם את כמות הימים באמצעות `plus`. אם נרצה לחשב תאריך בעבר, נוכל להשתמש במתודה `minus` באותו הדרך.

ניתן גם להשתמש במתודות נוספות כגון `with` ו- `until` על מנת לייצר תאריכים מודולרים ולבצע השוואות בין תאריכים.

לדוגמה, נוכל להשוות בין תאריך קיים לתאריך מודולרי על ידי השוואת החודשים ביניהם:

```Kotlin
val date1 = LocalDate.now()
val date2 = date1.withMonth(12)
if (date2.isBefore(date1)) {
    println("תאריך 2 קודם לתאריך 1")
}
```

נוכל גם להשתמש במתודת `until` כדי לחשב את מספר הימים או החודשים בין תאריכים:

```Kotlin
val date1 = LocalDate.now()
val date2 = date1.plusMonths(5)
val daysBetween = date1.until(date2, ChronoUnit.DAYS)
```

## העמקה

חישוב תאריך בעתיד או בעבר יכול להיות מעט מאתגר, במיוחד כאשר מדובר בתאריכים מודולריים או בחישובים שלוקחים בחשבון זמני קיץ וחורף. אם אתם מוכנים להתאמץ וללמוד עוד על הנושא, ניתן להתעמק במסמך הרשמי של Kotlin שמסביר בפירוט כיצד לבצע חישובים תאריכים בשפה זו.

## ראה גם

- [מסמ
---
title:    "Kotlin: קבלת התאריך הנוכחי"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

ישנם רבים שמתעניינים בתחום של תאריך ושעה נוכחיים בקוד, וזה בעיקר בגלל שזה מאפשר לאתר את הזמן שבו חלק מהקוד נכתב או יושלם. כמו כן, תאריך ושעה נוכחיים יכולים לשמש כמדד זמן חשוב ליצירת דוחות וניתוחים.

## איך לעשות זאת

השתמשו בפונקציה "LocalDate.now()" כדי לקבל את התאריך הנוכחי בפורמט של LocalDate. לדוגמה, מסתכלים על קוד הפלט של הפונקציה:

```Kotlin
val currentDate = LocalDate.now()
println(currentDate)
```

פלט:

```
2021-06-17
```

תוכלו גם להשתמש בפונקציה "LocalDateTime.now()" כדי לקבל גם שעה נוכחית. נסו את הקוד הבא:

```Kotlin
val currentDateTime = LocalDateTime.now()
println(currentDateTime)
```

פלט:

```
2021-06-17T09:19:28.931
```

אם אתם רוצים לקבל מידע מפורט יותר על התאריך והשעה הנוכחיים, ניתן להשתמש בפונקציית "format" עם תבנית ספציפית בתוכנית המדפסת. לדוגמה:

```Kotlin
val currentDateTime = LocalDateTime.now()
val formattedDateTime = currentDateTime.format(DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss"))
println(formattedDateTime)
```

פלט:

```
17/06/2021 09:21:50
```

## העמקה

כאשר עוסקים בתאריך ושעה, חשוב לזכור כי ישנם גם מצבים שבהם תאריך ושעה לא נכונים. זה עלול לקרות כאשר המחשב שבו כתוב הקוד פועל לא מוגדר כהתאריך נכון, או כאשר ישנם שגיאות בקוד שמשפיעות על התאריך והשעה הנוכחיים.

כדי למנוע שגיאות כאלו, יש להשתמש בפונקציות פנימיות של הטיפוס "LocalDate" ו"LocalDateTime" של Kotlin, אשר מציעות תמיד את התאריך והשעה הנכונים ביותר עבור המחשב הנוכחי.

## רא
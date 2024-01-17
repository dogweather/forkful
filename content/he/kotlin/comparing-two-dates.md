---
title:                "השוואת שתי תאריכים"
html_title:           "Kotlin: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
להשוות שתי תאריכים הוא פעולה שמטרתה להשוות בין שני תאריכים כדי לקבוע מי מהם הוא ישן יותר או אם הם זהים. מתכנתים משתמשים בפעולה זו כדי לבצע תנאים לוגיים, לקבוע את הטווח של אירועים או לנהל טבלאות.

## איך לעשות זאת:
### תאריך ושעה:
```Kotlin
val date1 = Calendar.getInstance()
date1.set(2020, Calendar.NOVEMBER, 25)

val date2 = Calendar.getInstance()
date2.set(2021, Calendar.JANUARY, 1)

if (date1.before(date2)) {
    println("תאריך 1 אחרי תאריך 2")
} else if (date1.after(date2)) {
    println("תאריך 1 לפני תאריך 2")
} else {
    println("שני התאריכים זהים")
}
```
פלט:
```
תאריך 1 לפני תאריך 2
```

## נחישות:
פעולת השוואת תאריך היא חלק חשוב מאוד בתכנות. זה נותן לנו את היכולת לשוות תאריכים על פני טווחים שונים ולהתאים אירועים לפי המרוויח המתאים ביותר. ניתן להשתמש בפעולה זו גם בתור מהדרך לנהל אירועים, למשל להרחיב את הטווח של מכירות או להקצות זמן לפי מועדו.

## ראה גם:
למידע נוסף על השוואה של תאריכים בכל הקוד הפייתון, שווה לך לבדוק את הקישורים הבאים:
- <https://kotlinlang.org/docs/reference/comparison-operations.html>
- <https://www.programiz.com/kotlin-programming/examples/compare-date>
- <https://www.geeksforgeeks.org/kotlin-different-ways-compare-two-dates/>
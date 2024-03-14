---
date: 2024-01-26 04:09:18.509791-07:00
description: "\u05E6\u05DC\u05D9\u05DC\u05D4 \u05DC\u05EA\u05D5\u05DA \u05DE\u05E0\
  \u05EA\u05D7 \u05D4\u05D5\u05D0 \u05DB\u05D5\u05DC\u05D5 \u05E2\u05E0\u05D9\u05D9\
  \u05DF \u05E9\u05DC \u05DC\u05E6\u05E2\u05D5\u05D3 \u05D3\u05E8\u05DA \u05D4\u05E7\
  \u05D5\u05D3 \u05E9\u05DC\u05DA, \u05DC\u05E8\u05D0\u05D5\u05EA \u05D0\u05EA \u05D4\
  \u05D2\u05DC\u05D2\u05DC\u05D9\u05DD \u05DE\u05E1\u05EA\u05D5\u05D1\u05D1\u05D9\u05DD\
  \ \u05D5\u05DC\u05EA\u05E4\u05D5\u05E1 \u05D0\u05EA \u05D4\u05D1\u05D0\u05D2\u05D9\
  \u05DD \u05D4\u05DE\u05E2\u05E6\u05D1\u05E0\u05D9\u05DD \u05D1\u05D9\u05D3\u05D9\
  \u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\
  \u05E9\u05D9\u05DD \u05D1\u05DE\u05E0\u05EA\u05D7\u05D9\u05DD \u05DB\u05D9 \u05D4\
  \u05DD \u05D4\u05DB\u05DC\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.278673-06:00'
model: gpt-4-0125-preview
summary: "\u05E6\u05DC\u05D9\u05DC\u05D4 \u05DC\u05EA\u05D5\u05DA \u05DE\u05E0\u05EA\
  \u05D7 \u05D4\u05D5\u05D0 \u05DB\u05D5\u05DC\u05D5 \u05E2\u05E0\u05D9\u05D9\u05DF\
  \ \u05E9\u05DC \u05DC\u05E6\u05E2\u05D5\u05D3 \u05D3\u05E8\u05DA \u05D4\u05E7\u05D5\
  \u05D3 \u05E9\u05DC\u05DA, \u05DC\u05E8\u05D0\u05D5\u05EA \u05D0\u05EA \u05D4\u05D2\
  \u05DC\u05D2\u05DC\u05D9\u05DD \u05DE\u05E1\u05EA\u05D5\u05D1\u05D1\u05D9\u05DD\
  \ \u05D5\u05DC\u05EA\u05E4\u05D5\u05E1 \u05D0\u05EA \u05D4\u05D1\u05D0\u05D2\u05D9\
  \u05DD \u05D4\u05DE\u05E2\u05E6\u05D1\u05E0\u05D9\u05DD \u05D1\u05D9\u05D3\u05D9\
  \u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\
  \u05E9\u05D9\u05DD \u05D1\u05DE\u05E0\u05EA\u05D7\u05D9\u05DD \u05DB\u05D9 \u05D4\
  \u05DD \u05D4\u05DB\u05DC\u05D9\u05DD\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D3\u05D9\u05D1\u05D0\u05D2\u05E8"
---

{{< edit_this_page >}}

## מה ולמה?
צלילה לתוך מנתח הוא כולו עניין של לצעוד דרך הקוד שלך, לראות את הגלגלים מסתובבים ולתפוס את הבאגים המעצבנים בידיים. מתכנתים משתמשים במנתחים כי הם הכלים הבלשיים שעוזרים לנו להבין איפה הדברים הולכים לא כשורה, מבלי לעקור את שערנו.

## איך לעשות:
הנה טעימה קטנה של ניתוח באגים ב-Kotlin עם IntelliJ IDEA - שרלוק הולמס של סביבות הפיתוח:

```kotlin
fun main() {
    val mysteryNumber = 42
    var guess = 0

    while (guess != mysteryNumber) {
        println("נחש את המספר: ")
        guess = readLine()?.toIntOrNull() ?: continue // התעלם מקלטים לא תקניים

        // הצב נקודת עצירה כאן כדי לראות את 'נחש' בפעולה
        if (guess < mysteryNumber) {
            println("נמוך מדי!")
        } else if (guess > mysteryNumber) {
            println("גבוה מדי!")
        }
    }

    println("הצלחת! המספר המסתורי הוא $mysteryNumber")
}
```

פלט המנתח:
```
נחש את המספר: 
10
נמוך מדי!
נחש את המספר: 
50
גבוה מדי!
נחש את המספר: 
42
הצלחת! המספר המסתורי הוא 42
```

## צלילה עמוקה
מנתחים נמצאים במשחק מהשנות ה-50. באותם זמנים, הם היו די פרימיטיביים, וניתוח באגים יכול היה להיות יותר על חומרה מאשר על תוכנה. כיום, מנתח כמו זה שב-IntelliJ IDEA מאפשר לנו להציב נקודות עצירה, לעבור דרך הקוד שורה אחר שורה, ולבדוק את מצב המשתנים לנוחותנו.

עוד כשמנתח ה-IntelliJ הוא מאוד שימושי עבור Kotlin, הוא לא הדג בים. יש מגוון אלטרנטיבות כמו Logcat לפיתוח אנדרואיד, או כלים מבוססי קווי פקודה כמו jdb עבור המינימליסטים. הקסם מאחורי הקלעים כאן הוא בעיקר על Interface Tool כלי JVM (JVMTI), שמאפשר למנתחים להתקשר עם מכונת הווירטואלית של ג'אווה, ומשאיר את מפתחי Kotlin במעגל.

## ראו גם
- תיעוד מנתח IntelliJ IDEA: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)

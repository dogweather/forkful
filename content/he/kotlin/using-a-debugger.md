---
title:                "שימוש בדיבאגר"
aliases:
- he/kotlin/using-a-debugger.md
date:                  2024-01-26T04:09:18.509791-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בדיבאגר"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/using-a-debugger.md"
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

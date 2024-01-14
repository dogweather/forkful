---
title:    "Kotlin: יצירת קובץ זמני"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

כיוון שיצירת קובץ זמני יכול להיות כלי מאוד חשוב בתהליכי קידום פיתוח בתוכניות קוד מבוססות Kotlin.

## איך לעשות

בקוד הבא נראה איך ליצור קובץ זמני בשפת קוד Kotlin ואיך לנהל אותו בזמן ריצה:
```Kotlin
val temporaryFile = File.createTempFile("kotlin_blog_post", ".txt")
temporaryFile.writeText("זוהי תוכן של קובץ זמני בשפת Kotlin!")
println(temporaryFile.readText())
temporaryFile.delete()
```

פלט של הקוד הנ"ל יהיה:
```
"זוהי תוכן של קובץ זמני בשפת Kotlin!"
```

## חקירה מעמיקה

יצירת קובץ זמני נעשית בעזרת הפונקציה `createTempFile` של מחלקת `File` בשפת Kotlin. מדוע זה נחשב לכלי חשוב בתהליך קידום פיתוח? כיוון שקבצים זמניים מאפשרים למפתחים לבצע בדיקות מהירות ולעבוד עם קבצים שמכילים נתונים זמניים בלבד, תוך כדי שמתחילים לפתח את התוכנית הסופית.

בנוסף, יצירת קובץ זמני מאפשרת למפתחים לבדוק פעולות קבצים, כגון כתיבה וקריאה, בפועל בלי לחבר את התוכנית לקבצים קיימים במערכת הקבצים. זה מבטיח כי הקוד עובד כצפוי ומציב את הבסיס לפיתוח התוכנית הסופית.

## ראה גם

- המדריך הרשמי לשפת Kotlin: https://kotlinlang.org/docs/home.html
- מדריך תיעוד של מחלקת File ב-Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/-file/
---
title:                "בדיקה האם תיקייה קיימת"
date:                  2024-01-20T14:57:17.758194-07:00
html_title:           "Gleam: בדיקה האם תיקייה קיימת"
simple_title:         "בדיקה האם תיקייה קיימת"

category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?

בדיקת קיום תיקייה במערכת הקבצים היא לבדוק אם תיקייה מסוימת כבר נמצאת במקום שהיא אמורה להיות. תוכניתאים עושים את זה כדי למנוע טעויות ריצה ולוודא שהנתונים שבתיקייה יטופלו כשהם זמינים.

## איך לעשות:

```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("/some/directory/path")

    if (Files.exists(path)) {
        println("The directory exists!")
    } else {
        println("The directory does not exist.")
    }
}
```

תוצאת דוגמה:

```
The directory exists!
// או
The directory does not exist.
```

## עיון מעמיק:

בעבר, פונקציות כמו `File().exists()` היו שיטת הבדיקה הנפוצה. עם השנים, ספריית NIO (New IO) נכנסה לתמונה, שמציעה גישה עדינה יותר וייעול טוב יותר למערכת הקבצים. חלק מסיבות השימוש ב-NIO הן יכולות ניהול קבצים מתקדמות ובדיקות הרשאות. בנוסף, כשמשתמשים ב`Files.exists()`, אפשר גם לציין אופציות בדיקה נוספות, כמו `NOFOLLOW_LINKS` למניעת עקיבה אחר קישורים סימבוליים.

## ראו גם:

- [דוקומנטציה של Java NIO Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- [מדריך אודות New I/O (NIO) ב-Oracle](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)
- [סטאק אוברפלו: איך לבדוק אם קובץ או תיקייה קיימים בג'אווה?](https://stackoverflow.com/questions/1816673/how-do-i-check-if-a-file-exists-in-java)

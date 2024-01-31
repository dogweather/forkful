---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
date:                  2024-01-19
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה ל-Standard Error (stderr) משמשת לדיווח על שגיאות ולצורך לוגים במערכות יישומים. תכניתנים משתמשים בכך כדי להפריד בין פלט רגיל לפלט שגיאות, וזה עוזר לדיבאגינג וניטור.

## איך לעשות את זה:
קטע קוד לדוגמא ופלט:
```Kotlin
fun main() {
    println("שלום, זה פלט תקין")
    System.err.println("שגיאה: משהו השתבש!")
}
```
פלט דוגמא:
```
שלום, זה פלט תקין
שגיאה: משהו השתבש!
```

## צלילה לעומק:
בעבר, פלטי התקן (stdout) ושגיאת התקן (stderr) נוצרו כדי לאפשר ניהול נכון יותר של פלט מחשבים מולטי-משימתיים. אלטרנטיבות כוללות כתיבה לקובץ לוג נפרד או שימוש במערכת לוגים חיצונית. כשכותבים ל-stderr בקוטלין, מאחורי הקלעים נעשה שימוש ב-`System.err`, המטפל בפלט שגיאות בפלטפורמות השונות שקוטלין תומך בהן.

## ראו גם:
- [מדריך רשמי לקוטלין](https://kotlinlang.org/docs/home.html)
- [מדריך להפרדת פלטים במערכות יוניקס](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
- [מאמר על טיפול בשגיאות בקוטלין](https://kotlinlang.org/docs/exceptions.html)

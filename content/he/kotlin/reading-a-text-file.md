---
title:    "Kotlin: קריאת קובץ טקסט"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

#למה

קובץ טקסט הוא אחד מסוגי הקבצים הנפוצים ביותר בתחום התכנות, כך שלימוד כיצד לקרוא קובץ טקסט בקוד Kotlin יהיה מאוד שימושי עבור מתכנתים המכוונים ליצור יישומים או כלים שעובדים עם קבצים.

# איך לעשות זאת

בכדי לקרוא קובץ טקסט בקוד Kotlin, ניתן להשתמש בפונקציה שנקראת "readText", כאשר נמצאים בתוך מחלקת "java.io.File". להלן דוגמה פשוטה:

```Kotlin
import java.io.File

fun main() {
    val file = File("myFile.txt")
    val text = file.readText()
    println(text)
}
```

תוצאה:
```
זהו קובץ טקסט פשוט
```

כדי לקרוא קובץ קשתוחיות יותר, ניתן להשתמש במתודה "forEachLine" שבתוך פונקצית הקביעה לקובץ. זאת מאפשרת לנו לעבור על כל שורות הקובץ ולבצע פעולה על כל אחת מהם. לדוגמה:

```Kotlin
import java.io.File

fun main() {
    val file = File("myFile.txt")
    file.forEachLine { println(it) }
}
```

תוצאה:
```
שורה 1
שורה 2
עוד שורה
```

# העומק

קריאת קובץ טקסט גורמת למחשב לטעון את הקובץ לזיכרון ולעבוד איתו כך שייתכן ותציג התנהגות פחות יעילה עבור קבצים גדולים. במקרה כזה, ניתן להשתמש בפתרון יעיל יותר כמו קריאת הקובץ מתוך עיבוד חוצה שעובר על הקובץ בפעם אחת במקום פעם כל איבר.

# ראה גם

- [תיעוד רשמי של Kotlin - קובץ](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [קריאת קבצי טקסט גדולים בכינוי קטן בשפת קוטלין](https://medium.com/@eliran139/reading-huge-text-files-in-kotlin-w-a-small-memory-footprint-a416de34bc36)
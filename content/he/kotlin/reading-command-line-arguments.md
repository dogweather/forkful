---
title:    "Kotlin: קריאת ארגומנטים בשורת הפקודה"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## למה:

קריאת פרמטרי שורת הפקודה היא כלי חשוב בכתיבת תוכניות עם שפת קוטלין. היא מאפשרת לנו לקבל קלט מהמשתמש ישירות מפקודת הטרמינל ולהשתמש בו בתוך התוכנית שלנו. זה מאפשר לנו להתאים את פעולת התוכנית לפי הקלט שנקבל וליצור חוויית משתמש טובה יותר.

## איך לעשות זאת:

תוכניות בקוטלין משתמשות בקלות רבה בפרמטרים של שורת הפקודה. כדי לקרוא אותם בקלות, אנו משתמשים באובייקט ה-mutableMap שבתוך מחלקת ה-main- של התוכנית שלנו. בקלות אנו מקבלים את כל הפרמטרים כמחרוזת ואנו יכולים לעבד אותם בקלות. תוכלו לראות דוגמא לעיבוד פרמטרים בקוד שלמטה:

```Kotlin
fun main(args: Array<String>) {
    val commandArgs = mutableMapOf<String, String>()
    for (arg in args) {
        if (arg.startsWith("--")) {
            val (argName, argValue) = arg.split("=")
            commandArgs[argName.substring(2)] = argValue
        }
    }
    
    println("שם: ${commandArgs["שם"]}")
    println("גיל: ${commandArgs["גיל"]}")
}
```

פנקס עבודות מחולל פקודות יחסוך לנו את הכתיבה של קבועת-גיל ארוכה ומשוכללת ויוצר אותה באופן אוטומטי כאשר נכתוב קוד בפנקס ונפעילו. ניתן להוסיף פרמטרים נוספים בפנקס שניתן יהיה לגשת אליהם בסדר מסוים או לקבל ערך באמצעות הפונקציה readLine ולהכניס את הפרמטרים לבד.

```Kotlin
@Parameters(autoNameDetection = true)
data class JobResult(val logFile: String, val user: Boolean)

fun main(args: Array<String>) {
    val cmd = args
        .map { it.name to it }
        .toMap()
    
    val jobResult = JobResult(
        cmd["--name"] ?: error("Although arg --name is mandatory it does not happen."),
        cmd["--user"] != null
    )
    
    println("שם קובץ לוג: ${jobResult.logFile}")
    println("משתמש: ${jobResult.user}")
}
```

## מעמד מעמ
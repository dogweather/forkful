---
title:                "הסרת מרכאות ממחרוזת"
date:                  2024-01-26T03:41:24.931847-07:00
model:                 gpt-4-0125-preview
simple_title:         "הסרת מרכאות ממחרוזת"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

הסרת מרכאות ממחרוזת אומרת להסיר את כל מופעי התווים של המרכאות, בין אם אלו מרכאות יחידות (' ') או כפולות (" "), מנתוני הטקסט שעימם אתם עובדים. מתכנתים לעיתים קרובות צריכים לעשות זאת לניקוי נתונים, להכנתם לעיבוד נוסף, או כאשר המרכאות עצמן אינן רלוונטיות למשמעות הנתונים.

## איך לעשות:

הנה דרך פשוטה להסיר שני סוגי המרכאות ממחרוזת בKotlin:

```kotlin
fun removeQuotes(input: String): String {
    return input.replace("\"", "").replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    val stringWithoutQuotes = removeQuotes(stringWithQuotes)
    println(stringWithoutQuotes) // פלט: Kotlin rocks its cool
}
```

ואם אתם רוצים להסיר רק סוג אחד של מרכאות, פשוט תדלגו על הקריאה השנייה להחלפה.

```kotlin
fun removeDoubleQuotes(input: String): String {
    return input.replace("\"", "")
}

fun removeSingleQuotes(input: String): String {
    return input.replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    println(removeDoubleQuotes(stringWithQuotes)) // פלט: Kotlin rocks it's 'cool'
    println(removeSingleQuotes(stringWithQuotes)) // פלט: Kotlin "rocks" its cool
}
```

## עיון נוסף

מבחינה היסטורית, טיפול במחרוזות ובתווים מיוחדים היה חלק יסודי מתכנות, כיוון שטקסט הוא דרך יסודית בה אנו מגשרים עם נתונים. לעיתים קרובות יש צורך להימלט ממרכאות בתוך מחרוזות. זה מסומן על ידי קו נטוי אחורי מוקדם (לדוגמה, `"היא אמרה, \"היי!\""`). כאשר מעבדים מחרוזות כאלו, ייתכן שתצטרכו להסיר את התווים המיוחדים, או את המרכאות עצמן לקבלת טקסט נקי או נוח יותר לשימוש.

חלופות לשיטת ה`replace` כוללות הסרה מבוססת regex או ניתוח ידני של המחרוזת, תו אחר תו. עם זאת, regex יכולה להיות יתר על המידה לפעולות פשוטות וניתוח ידני הוא פחות יעיל משימוש בפונקציות מחרוזת מובנות. פונקציית ה`replace` של Kotlin מנצלת את שיטת ה`replace` של מחרוזת בJava, שמאוד מיועלת לביצועים.

מבחינת היישום, כדאי לציין שKotlin מתפעלת עם Java, ולכן, למעשה, כל פעולה שאתם מבצעים על מחרוזות תהיה ביצועית כמו שהייתה בJava. חשוב להיות מודעים למקרי קצה כשמסירים מרכאות, כמו מרכאות מקוננות, שעשויות לדרוש גישה מתוחכמת יותר, אולי באמצעות ביטויים רגולריים או ספריית ניתוח.

## ראה גם

למידע נוסף על טיפול במחרוזות בKotlin, ניתן לבדוק את התיעוד הרשמי:

- [תיעוד הString של Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [עבודה עם מחרוזות בKotlin](https://play.kotlinlang.org/byExample/02_control_flow/06_String%20Templates)

לעיונים עמוקים יותר בביטויים רגולריים וניתוח בKotlin:

- [תיעוד Kotlin Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [ניתוח טקסט בKotlin](https://typealias.com/start/parsing-in-kotlin/)
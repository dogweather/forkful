---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים שמתאימים לדפוס היא תהליך שבו מוחקים תווים מסוימים ממחרוזת, בהתאם לדפוס שהוגדר מראש. מתכנתים עושים זאת לניקוי ולקידום מחרוזות נתונים, להימנע מטעויות ולשפר את ביצועי הקוד.

## איך לעשות:
ישנם מספר דרכים למחוק תווים שמתאימים לדפוס ב-Kotlin. דוגמה אחת היא עם שימוש בפונקציה `replace()`:
```Kotlin
fun main() {
    val str = "אני אוהב לתכנת ב-Kotlin!"
    val pattern = "[א-ת ]".toRegex()
    val newStr = str.replace(pattern, "")
    println(newStr)  // "Kotlin!"
}
```
בקוד הזה, הדפוס `[א-ת ]` מציין את כל האותיות העבריות והרווח. הפונקציה `replace()` מחזירה מחרוזת חדשה בה כל התווים שמתאימים לדפוס הוחלפו.

## צלילה עמוקה
תהליך מחיקת תווים התאמת דפוס נשען על טכנולוגיה שנקראת "ביטויים רגולריים" שפותחה במהלך שנות ה-50'.
ישנן אלטרנטיבות ל-RexEx, כמו DFA ו-NFA, אך RexEx הפכו להיות הפופולריים ביותר בשל הנוחות שלהם.
בנוסף, Kotlin משתמש במנגנון של JVM למימוש של Regex, מה שאומר שהפונקציה `replace()` מתבצעת על JVM String, ולא על Kotlin native String.

## ראה גם
- אתר הרשמי של תיעוד Kotlin regex: [link](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- קורס אינטרנטי בנושא Regex עם דוגמאות ב-Kotlin: [link](https://www.coursera.org/courses?query=kotlin%20regular%20expressions)
- ספר של O'Reilly המתמקד ב-Regex בתכנות: [link](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
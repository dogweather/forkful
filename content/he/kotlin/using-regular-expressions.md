---
title:                "שימוש בביטויים רגולריים"
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
רגקס (RegEx, Regular Expressions) זה שפת תבנית לחיפוש טקסט. מתכנתים משתמשים בה לסינון, חיפוש, או שינוי טקסט בצורה יעילה ומדויקת.

## איך לעשות:
```Kotlin
fun main() {
    val text = "למדנו Kotlin בשנת 2023!"
    val regex = "\\d+".toRegex()
    
    val foundNumbers = regex.findAll(text).map { it.value }.joinToString()

    println("מספרים שנמצאו: $foundNumbers")  // Output: מספרים שנמצאו: 2023
}
```

הדוגמה תופסת מספרים בטקסט ומדפיסה אותם.

## צלילה לעומק:
רגקס הייתה בשימוש לראשונה בשנות ה-50 ופותחה על ידי קהילת Unix. חלופות כוללות פעולות עם מחרוזות סטנדרטיות אבל הן לרוב איטיות יותר ופחות גמישות. בקוטלין, RegEx מיושמת דרך כיתות regex עם מתודות כמו find, match, replace.

## ראה גם:
- [תיעוד קוטלין למחלקת Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [tutorialspoint - Kotlin Regular Expressions](https://www.tutorialspoint.com/kotlin/kotlin_regular_expressions.htm)
- [regex101](https://regex101.com/) - מחולל ומבדק רגקס אינטראקטיבי
---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
פלט ניפוי באגים הוא כלי שמשמש תוכנתיים לעקוב אחרי ביצוע התוכנית בידע מפורט בזמן הריצה. זה מאפשר לנו לזהות ולתקן באגים באזורים מסוימים של הקוד.

## איך מבצעים:
קוטלין מספקת פונקציה מובנית שנקראת "print" שאפשר למצוא ראשית תוכנית. להלן דוגמה:

```Kotlin
fun main() {
    val name = "Kotlin"
    print("Hello, $name!")
}
```
פלט יהיה: `Hello, Kotlin!`

## הצצה לעומק:
1. היסטוריה: "Debug output" הוא מתכנת מעוין מהימים הראשונים של הכתיבה, אף לפני שינוי של קוטלין. זה דרך שמתכנתים משתמשים בה לשים בחשיפה באגים שאפשר למצוא.
2. אלטרנטיבות: לרוב השפות יש מבנה פלט דומה. ב-Java, ניתן להדפיס דרך `System.out.println()`. ב-Python, ניתן להשתמש בפונקציה `print()`.
3. פרטי מימוש: במחשבים מודרניים, "print" עכשיו מחזיר פלט למסוף. בעבר, הייתה היכולת להדפיס את הפלט ישירות למדפסת!

## ראה גם:
2. [איך להדפיס ב-Java](https://www.w3schools.com/java/java_user_input.asp)
3. [פקודת הדפסה ב-Python](https://docs.python.org/3/tutorial/inputoutput.html)
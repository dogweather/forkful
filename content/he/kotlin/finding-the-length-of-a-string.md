---
title:                "Kotlin: מציאת אורך של מחרוזת"
simple_title:         "מציאת אורך של מחרוזת"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

#למה

בעולם התכנות, ישנם מספר רב של פעולות נפוצות שאנו מבצעים על מחרוזות. אחת מהפעולות הנפוצות ביותר היא לאתר את אורך המחרוזת. יכול להיות שאתם נתקלים בשאלה כזו כאשר אתם מבצעים טיפול בנתונים או כאשר אתם בונים אפליקציה. במאמר זה אנו נלמד כיצד למצוא את אורך המחרוזת בקוד Kotlin.

#כיצד

כדי למצוא את אורך המחרוזת ב-Kotlin, ניתן להשתמש בפונקציה `length`. נבצע דוגמה פשוטה:

```Kotlin
val string = "זוהי מחרוזת"
val length = string.length
println(length) // פלט: 11
```

בדוגמה זו, אנו משתמשים בפונקציה `length` כדי לאתר את אורך המחרוזת "זוהי מחרוזת". ניתן גם להשתמש בפונקציה זו בדומה לפעולות הנוספות של Kotlin:

```Kotlin
val length = "זהו טקסט".length
println(length) // פלט: 8
```

ניתן גם להשתמש במחרוזת ריקה ולמצוא את אורךה:

```Kotlin
val emptyString = ""
val length = emptyString.length
println(length) // פלט: 0
```

#מעמיקים יותר

אם אתם קצת מעוניינים להבין כיצד פונקציה זו מבצעת את עבודתה, אנו נכנסים עכשיו לתיאוריה. פונקציית `length` היא חלק מכיתת `CharSequence` של Kotlin, שכוללת פונקציות שימושיות לטיפול במחרוזות.

כאשר אנו קוראים לפונקציית `length` על מחרוזת מסוימת, היא מחזירה לנו את המספר הכולל של התווים במחרוזת כאורך. לדוגמה, אם ננסה להמיר פונקציה זו לאופרטור, היא תיראה כך:

```Kotlin
val length = string.length()
// כאן, הפונקציה `length()` מתבצעת על המחרוז
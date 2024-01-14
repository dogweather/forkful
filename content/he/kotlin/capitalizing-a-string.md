---
title:    "Kotlin: כתיבת מחרוזת רישיות"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# למה
במאמר זה, אנו נדבר על כיצד נוכל להפוך מחרוזת בקוד Kotlin לכתיב ראשוני גדול. כך תהיה לנו אפשרות להציג את המחרוזת בצורה יותר מסודרת ונוחה לקריאה.

# כיצד לעשות זאת
כדי להפוך מחרוזת לכתיב ראשוני גדול בקוד Kotlin, אנו ניצור משתנה חדש שיכיל את המחרוזת המקורית. לאחר מכן, נפעיל פונקציה הנקראת "capitalize" על המשתנה כדי להפוך את הכתיב לראשוני גדול. לאחר מכן, נדפיס את המשתנה החדש כך שנוכל לראות את התוצאה המתאימה.

```
Kotlin val str = "hello world"
val capitalizedStr = str.capitalize()
println(capitalizedStr)
```

הפלט של הקוד הנלווה יהיה:

```
Hello world
```

במקרה שבו נרצה להפוך רק את האות הראשונה במחרוזת לגדולה, נוכל להשתמש בפונקציה "replaceRange" כדי להחליף את האות הראשונה במחרוזת לכתיב גדול. הנה דוגמא לכך:

```
Kotlin val str = "hello world"
val capitalizedFirstLetterStr = str.replaceRange(0, 1, str.substring(0, 1).toUpperCase())
println(capitalizedFirstLetterStr)
```

הפלט של הקוד הנלווה יהיה:

```
Hello world
```

# חפירה עמוקה
הפונקציה "capitalize" מחזירה מחרוזת חדשה עם הכתיב ראשוני גדול, אך אינה משנה את המחרוזת המקורית. כאשר אנו משתמשים בפונקציה "replaceRange", אנו משנים את המחרוזת המקורית עצמה. גם פונקציות נוספות כמו "toTitleCase" ו-"toUpperCase" יכולות לשמש כדי להפוך את המחרוזות לכתיב ראשוני גדול.

כדי להיות יעילים יותר ולמנוע טעויות, נוכל לעצור על בדיקה תנאית של המחרוזת לפני שאנו מנסים להפוך אותה לכתיב ראש
---
title:    "Gleam: כתיבה לתקליטור סטנדרטי"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Gleam תשוׇות שגליים: למה לכתוב לפלט שגיאות

## למה
כתיבה לפלט שגיאות היא דרך נוחה לדבר ישירות למשתמש המפעיל את התוכנית שלך. זה יכול לעזור למשתמש להבין את הטעויות שלהם ולתקן אותן בדיוק.

## כיצד לעשות זאת
נוכל להשתמש בידע בשפת Gleam כדי להדפיס מידע לתוך פלט השגיאות באמצעות הפונקציה `standard_error.print`. הנה דוגמאות קטנות:

```Gleam
let error_message = "אופס! משהו השתבש"
standard_error.print(error_message)
```

פלט:
```
אופס! משהו השתבש
```

כדי לקבל מידע נוסף על שגיאות ספציפיות, נוכל להשתמש בפונקציה `format` כדי לציין כיצד להציג את המידע בפלט השגיאה. למשל:

```Gleam
let error_code = 404
let error_message = format("העמוד לא נמצא. קוד שגיאה: {}", [error_code])
standard_error.print(error_message)
```

פלט:
```
העמוד לא נמצא. קוד שגיאה: 404
```

## מעמקים
כתיבה לפלט שגיאות בשפת Gleam היא דרך מפתחת להתחברות עם משתמשי התוכנה שלכם. כשהם נתקלים בשגיאות, הם יוכלו להבין את הבעיה ולתקן אותה כדי להמשיך להשתמש בתוכנה שלכם. כמו כן, זה גם נותן למפתחים תצורה יותר טובה לניהול טעויות ותחזוקה מתקדמת.

## ראו גם
* [טיפול בשגיאות בשפת Gleam](https://gleam.run/articles/handling-errors/)
* [מדריך בנושא שגיאות ספציפיות](https://gleam.run/articles/typed-errors/)
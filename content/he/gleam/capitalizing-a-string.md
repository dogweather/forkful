---
title:    "Gleam: החלפת ראשי אותיות במחרוזת"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

דועה: למה משתמשים ב-Gleam לכבות מחרוזות.
כיצד: דוגמאות לקוד ותוצאות דוגמא מופיעות בתוך קוד Gleam:
"```Gleam
fn capitalize_string(string) {
    uppercase_string = String.to_uppercase(string)
    return uppercase_string
}

IO.println(capitalize_string("hello")) // הדפסת "HELLO"
IO.println(capitalize_string("gleam")) // הדפסת "GLEAM"
```"
עיון עמוק: מידע מעמיק על כיצד לכבות מחרוזות והצגת אפשרויות נוספות לשימוש בו ב-Gleam.
שים לב: כדאי להשתמש בטכניקות אחרות לכיבות מחרוזות באמצעות פונקציות נוספות כגון String.to_titlecase ו-String.to_lowercase כדי להשתמש בטכניקות נוספות לעיבוד טקסט ושפת תכנות רב עוצמה זו.

See Also:
למידע נוסף על Gleam וטכניקות נוספות לשימוש בו, נא להיכנס לאתרים המצוינים להלן:
- https://gleam.run
- https://github.com/gleam-lang/gleam
- https://gleam.io/docs
---
title:                "Gleam: הדפסת פלט ניפוי שגיאות בתכנות מחשבים"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

על מה זה כדאי: להדפיס מידע נוסף במהלך התהליך של בדיקת תקינות יכול לעזור במציאת באגים והתקלות בקוד.

איך לעשות את זה: דוגמאות קוד ופלט ללא בוגר שנכתבים בתוך כדות "Gleam...". לדוגמה:

```Gleam
fn main() {
   debug!("המשתנה הזה: {}", משתנה);
}
```
פלט: "המשתנה הזה: ערך המשתנה שלך".

עומק הבנתם: מדוע וכיצד להשתמש בהדפסת מידע נוסף כדי לזהות ולטפל בבאגים והתקלות בקוד שלך. ניתן להשתמש בהדפסת מידע בכל מקום בקוד שאתה רוצה לראות את הערכים של המשתנים שלך. ניתן גם להדפיס מידע נוסף כדי לבחון באיזו ערכת המשתנים אתה מייצג כדי לאתר ולתקן באגים. בנוסף, ניתן לשלב הדפסת מרחבי שינוי לכך שתוכל לפרוס נתונים בצורה ממוקדת יותר.

ראה גם: [בדיקת תקינות עם הדפסת מידע נוסף](https://gleam.run/articles/testing-with-debug-printing/), [הדפסת מידע נוסף יותר עם לוגים דינמיים](https://gleam.run/articles/dynamic-logging/), [מעקב ודוגמאות לכתיבת קוד ברור ומנטור](https://gleam.run/articles/writing-clean-and-maintainable-code/).
---
title:                "בדיקה האם תיקייה קיימת"
date:                  2024-01-20T14:56:13.000184-07:00
html_title:           "Gleam: בדיקה האם תיקייה קיימת"
simple_title:         "בדיקה האם תיקייה קיימת"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקת קיום תיקייה במערכת הפעלה היא פעולה שמאשרת אם תיקייה קיימת בנתיב מסוים. תכניתנים עושים זאת כדי למנוע שגיאות בעת ניסיון גישה לתיקייה שלא קיימת, או בכדי לבנות את התיקייה אם היא לא קיימת.

## איך לעשות:
```gleam
pub fn check_directory_exists(path: String) -> Bool {
  // קוד פה...
}

fn main() {
  let does_exist = check_directory_exists("path/to/your/directory")
  io.println(does_exist) // יודפס true או false
}
```

תוצאת הדוגמא:
```
true
```
או
```
false
```
אם התיקייה לא קיימת.

## צלילה עמוקה
בעבר, בדיקת קיום של תיקייה בכל שפת תכנות הייתה מעט שונה, אבל העיקרון כמעט תמיד זהה. ב-Gleam, ניתן להשתמש בפונקציות של מערכת ההפעלה דרך חבילות שמבצעות גישה ישירה ל-API של המערכת. חלופות כוללות יצירת התיקייה באופן יזום אם היא לא קיימת, או טיפול בשגיאה אם התיקייה לא נמצאת. היישום הפנימי משתנה בהתאם למערכת ההפעלה, אך גישה נפוצה היא בדיקת החזרה מקריאת מערכת שמנסה לגשת לתיקייה.

## ראה גם
- תיעוד רשמי של Gleam: [https://gleam.run](https://gleam.run)
---
title:                "Gleam: שרשור מחרוזות"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה?

קישור מחרוזות הוא כלי חיוני בתכנות, הוא מאפשר למפתחים ליצור מחרוזות ארוכות ומורכבות מתוך מחרוזות קטנות יותר. זה מאפשר לנו ליצור תוכניות בעלות יכולות מיוחדות ותיאומים מדוייקים, כגון יצירת הודעות למשתמש, יצירת פרסומות וכו'.

## איך לבצע קישור מחרוזות

``` Gleam
let name = "עמית"
let greeting = "שלום"

let message = greeting ++ ", " ++ name ++ "!"
// פלט: שלום, עמית!

let numbers = "123" ++ "456"
// פלט: 123456
```

## העמוד העמוק

קישור מחרוזות ב-Gleam מאפשר גם להשתמש במשתנים נוספים באמצעות תוויות מגדר. בנוסף, ניתן להשתמש בפונקציות כדי ליצור מחרוזת דינמית. כל מחרוזת מוגדרת כך שניתן לגשת אליה כפעולת התעתקות.

## ראה גם

- קישור מחרוזות ב-Gleam: https://gleam.run/documentation/guides/strings.html
- קוד דוגמה לקישור מחרוזות ב-Gleam: https://github.com/gleam-lang/gleam/blob/master/examples/strings.gleam
---
aliases:
- /he/elm/organizing-code-into-functions/
date: 2024-01-26 01:12:03.460480-07:00
description: "\u05DC\u05D6\u05E8\u05D5\u05E7 \u05D0\u05EA \u05DB\u05DC \u05D4\u05E7\
  \u05D5\u05D3 \u05E9\u05DC\u05DA \u05DC\u05EA\u05D5\u05DA \u05E2\u05E8\u05DE\u05D4\
  \ \u05D2\u05D3\u05D5\u05DC\u05D4 \u05D0\u05D7\u05EA? \u05E8\u05E2\u05D9\u05D5\u05DF\
  \ \u05E8\u05E2. \u05DC\u05E4\u05E8\u05E7 \u05D0\u05D5\u05EA\u05D5 \u05DC\u05EA\u05D5\
  \u05DA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA? \u05E8\u05E2\u05D9\u05D5\
  \u05DF \u05D8\u05D5\u05D1. \u05D6\u05D4 \u05DE\u05E9\u05DE\u05E8 \u05D0\u05EA \u05E7\
  \u05D5\u05D3 \u05D4-Elm \u05E9\u05DC\u05DA \u05E0\u05E7\u05D9, \u05E0\u05D9\u05EA\
  \u05DF \u05DC\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D7\u05D5\u05D6\u05E8, \u05D5\u05E7\
  \u05DC \u05D9\u05D5\u05EA\u05E8 \u05DC\u05D1\u05D3\u05D9\u05E7\u05D4.\u2026"
lastmod: 2024-02-18 23:08:52.759161
model: gpt-4-1106-preview
summary: "\u05DC\u05D6\u05E8\u05D5\u05E7 \u05D0\u05EA \u05DB\u05DC \u05D4\u05E7\u05D5\
  \u05D3 \u05E9\u05DC\u05DA \u05DC\u05EA\u05D5\u05DA \u05E2\u05E8\u05DE\u05D4 \u05D2\
  \u05D3\u05D5\u05DC\u05D4 \u05D0\u05D7\u05EA? \u05E8\u05E2\u05D9\u05D5\u05DF \u05E8\
  \u05E2. \u05DC\u05E4\u05E8\u05E7 \u05D0\u05D5\u05EA\u05D5 \u05DC\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA? \u05E8\u05E2\u05D9\u05D5\u05DF\
  \ \u05D8\u05D5\u05D1. \u05D6\u05D4 \u05DE\u05E9\u05DE\u05E8 \u05D0\u05EA \u05E7\u05D5\
  \u05D3 \u05D4-Elm \u05E9\u05DC\u05DA \u05E0\u05E7\u05D9, \u05E0\u05D9\u05EA\u05DF\
  \ \u05DC\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D7\u05D5\u05D6\u05E8, \u05D5\u05E7\u05DC\
  \ \u05D9\u05D5\u05EA\u05E8 \u05DC\u05D1\u05D3\u05D9\u05E7\u05D4.\u2026"
title: "\u05E1\u05D9\u05D3\u05D5\u05E8 \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
לזרוק את כל הקוד שלך לתוך ערמה גדולה אחת? רעיון רע. לפרק אותו לתוך פונקציות? רעיון טוב. זה משמר את קוד ה-Elm שלך נקי, ניתן לשימוש חוזר, וקל יותר לבדיקה. על ידי ארגון הקוד שלך לתוך פונקציות, אתה קבוצות קוד שמבצע משימות מסוימות יחד, שהופך את היישום שלך ליותר ניתן לתחזוקה ולהבנה.

## איך לעשות:
הנה חתיכת קוד עם פונקציה פשוטה לברך משתמש:

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String
greetUser userName =
    "Hello, " ++ userName ++ "!"

main =
    text (greetUser "Casey")
```

הרץ את זה, ותקבל את הפלט: "Hello, Casey!"

עכשיו, בוא נניח שאתה רוצה להוסיף פרטניזציה נוספת. הוצא יותר פונקציונליות!

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String -> String
greetUser greeting userName =
    greeting ++ ", " ++ userName ++ "!"

personalGreeting : String -> String
personalGreeting userName =
    greetUser "Howdy" userName

main =
    text (personalGreeting "Casey")
```

עכשיו, כשתריץ את זה: "Howdy, Casey!" קסם? לא, פשוט פונקציות עושות את שלהן.

## לעומק
בימים של פעם, הקוד לעתים קרובות היה רצף אחד ארוך של הוראות (חשבו על קוד ספגטי). זה היה סיוט לתחזוק. אז הגיע התכנות המובנה, וגם פונקציות. Elm, כמו אבות תכנות פונקציונליים שלפניו, מסתמך כבדות על פונקציות לארגון.

אתה יכול לשכנע פונקציות, יוצר סגורות, או לשמור עליהם טהורות למען פשטות. Elm מעודדת את האחרון: פונקציות טהורות עם קלטים ופלטים מוגדרים היטב, מה שמוביל לאיתור באגים ובדיקות קלים יותר.

פונקציות ב-Elm יכולות גם להיות מסדר גבוה יותר, משמע שהן יכולות לקבל או להחזיר פונקציות אחרות. זה פותח עולם שלם של אפשרויות להרכבה. עם זאת, בניגוד לחלק מהשפות האחרות, ב-Elm אין טעינת פונקציות; לכל פונקציה חייב להיות שם ייחודי.

בנוסף, Elm מכתיבה מערכת טיפוסים סטטית חזקה שלא רק בודקת את הטיפוסים אלא גם מנבאת אותם, ובכך מפחיתה את הצורך בקוד פשוט.

כאשר משווים לחלופות כמו ארגון קוד פרוצדורלי או מונחה-עצמים בשפות אחרות, גישת Elm מתמקדת בפשטות ובניתן לחיזוי. ב-Elm אין עצמים או מחלקות. אתה מארגן קוד עם פונקציות ומודולים במקום מחלקות ומופעים.

## ראה גם
כדי לחקור עוד יותר, בדוק את המשאבים הללו:
- המדריך הרשמי של Elm על פונקציות: https://guide.elm-lang.org/core_language.html
- תיעוד חבילת Elm לדוגמאות פונקציות מורכבות יותר: https://package.elm-lang.org/
- למידה על מערכת הטיפוסים של Elm, שמשתלבת יפה עם ארגון פונקציות: https://elm-lang.org/docs/types

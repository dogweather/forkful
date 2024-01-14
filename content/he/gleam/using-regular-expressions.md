---
title:                "Gleam: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה

רק 1-2 משפטים המסבירים *למה* מישהו ישתמש בביטויים רגילים.

ביטויים רגילים הינם כלי עוצמתי וחשוב בתכנות ונעשה שימוש בהם כדי לזהות ולעבד טקסט באופן יעיל וגמיש. הם מאפשרים לנו לחפש ולהחליף דפוסים, לבצע ולקבל נתונים מבחר של מחרוזות, ולעצב תבניות מדויקות לתאים מסוימים של מחרוזות.

## איך לעשות זאת

תרשים הבא מראה כיצד להשתמש בביטויים רגילים ב-Gleam כדי לחפש ולהחליף את האותיות "a" ו"e" במחרוזת נתונה:

```Gleam
import gleam/re as re
import gleam/string as string

let text = "Hello, Gleam!"
let regex = re.compile("[ae]")
let result = re.replace(regex, text, fun(_) -> "u")
string.uppercase(result)
```
פלט: "Hullo, Gluum!"

ניתן גם להשתמש בביטויים רגילים ב-Gleam כדי לבצע בדיקת תקינות על מחרוזת. לדוגמה, התכנית הבאה בודקת אם מחרוזת נתונה הינה כתובת אימייל תקינה:

```Gleam
import gleam/re as re
import gleam/string as string

let email = "example@email.com"
let regex = re.compile("^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,4}$")
let result = re.match(regex, email)
string.to_string(result)
```
פלט: "Ok"

בדוגמאות אלו, הכנסנו את שם המודול "gleam/re" בכדי להשתמש בפונקציות המתאימות ב-Gleam לעבודה עם ביטויים רגילים.

## בילוי עמוק

כאשר משתמשים בביטויים רגילים ב-Gleam, יש לקחת בחשבון כמה דברים חשובים:

1. פונקציות הפעלה - המודול "gleam/re" מציע הפעלות סילוק, חפש והחלף, החלף כתובת דוואר אלקטרוני חסר מפעל סילוק, תאריך כתוב אנגלי וכו 'ולמשתמשים.
2. תבניות - מציאת מ
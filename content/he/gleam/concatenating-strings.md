---
title:                "שרשור מחרוזות"
date:                  2024-01-20T17:34:42.322191-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרשור מחרוזות"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?
הדבקת מחרוזות היא תהליך שבו שתי מחרוזות או יותר מתחברות זו לזו למחרוזת אחת. תוכניתנים עושים זאת כדי לייצר טקסטים דינמיים ולנהל נתונים טקסטואליים בצורה יעילה.

## איך לעשות:
ב-Gleam, הדבקת מחרוזות נעשית בעזרת הפונקציה `String.concat`. נסו זאת:

```gleam
import gleam/string.{concat}

fn main() {
  let greeting = concat(["שלום", " ", "עולם!"])
  greeting
}
```

במחברת זו תצא לכם המחרוזת `שלום עולם!`.

## צלילה עמוקה:
הדבקת מחרוזות היא פעולה בסיסית ברוב שפות התכנות וחלק מהן מספקות סינטקס מיוחד לכך, כמו '+' ב-JavaScript או Perl. ב-Gleam, שפה צעירה שהתפתחה ממערכת ה-Erlang VM, משתמשים בפונקציה `concat` שמחזיקה נכונות פונקציונלית. בשונה מסתם חיבור של מחרוזות, `concat` יכולה להתמודד עם מערכים של מחרוזות ולא רק עם זוגות. זה עשוי לשמור על ביצועים ולמנוע בזבוז זיכרון בעת עיבוד מחרוזות גדולות.

## ראה גם:
- [Gleam's Official Getting Started Guide](https://gleam.run/book/tour/)
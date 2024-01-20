---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מהי הפכנת תאריך למחרוזת ולמה היא נדרשת?

הפכנת תאריך למחרוזת היא התרגום של משתנה מסוג תאריך למחרוזת, יכול להיות שזה לצורך הצגה או לשמירה במסד נתונים. בפרוגרמינג הפיכנו מאפשרת לנו לבצע מניפולציות מחרוזות על ערכים של תאריך וזמן.

## איך מבצעים את ההמרה: 
אליקסיר מכילה פתרון תוך כדי חבילת סטנדרט שנקראת `DateTime`, שמכילה פונקציה בשם `DateTime.to_string/1` שמומרת תאריך למחרוזת. ראה כיצד זה עובד:

```Elixir
d = DateTime.utc_now()
DateTime.to_string(d)
```
תוצאה:

```Elixir
"2022-06-26 10:10:00Z"
```
## בהרחבה:
### הקשר ההיסטורי:
מדובר במחזור תכנות נפוץ שנפוץ בשפות אחרות כמו Python, Java, או JavaScript. אליקסיר מציעה חבילה משובחת עם פונקציונליות רחבה בעזרת `DateTime`.

### חלופות:
אליקסיר מציעה גם חבילות חיצוניות כמו `Timex` שמציעה תוספות נוספות שנותנות יותר גמישות בהתמודדות עם תאריכים ומחרוזות.

### פרטי מימוש: 
בתוך `DateTime.to_string/1`, נעשה שימוש בפונקציה 'Calendar.ISO' שבאה עם ה- mix מותקן של Elixir. באופן נעלם למשתמש, הפונקציה מנהלת את הפורמט של המחרוזת.

## ראו גם:
טימקס: [https://hexdocs.pm/timex/Timex.html](https://hexdocs.pm/timex/Timex.html)

עמוד התיעוד של DateTime:
[https://hexdocs.pm/elixir/DateTime.html](https://hexdocs.pm/elixir/DateTime.html)
---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
השוואת שני תאריכים היא תהליך שבו מורידים את ההיפרבולה של שני תאריכים על מנת להבין איזה מהם מגיע לפני השני. תכנתים מבצעים זאת כדי לבצע סינון, מיון וארגונית הנתונים לפי תאריך. 

## איך לעשות:
תוכל להשתמש בפונקציות של הספריות `gleam/date_time` להשוות בין שני תאריכים. חלק מהקוד שאנחנו מראים כאן הוא:

```Gleam
import gleam/date_time.{ Date, equal }

let date1 = date_time.from_iso8601("2022-01-01")
let date2 = date_time.from_iso8601("2022-01-02")
equal(date1, date2)
```
הערך לאחר פעולת ההשוואה יהיה `False`, שכן התאריכים אינם שווים.

## בהקשר עמוק יותר:
לעת עתה, Gleam לא מסופק עם ספרייה מובנית לתאריכים. לכן אנחנו משתמשים בספריית `gleam/date_time` שפותחה על ידי קהילה. אנחנו מצפים כי פונצ'קציות של ספרייה זו תיצאו בגרסה הבאה של Gleam. 
לחלופין, במקרה של אפליקציות מורכבות יותר, יכול להיות שתרצה לשקול שימוש בספרייה כמו `gleam/stdlib/date` שמספקת תמיכה בזמנים של אזוריות שונים.
פרטי היישום: אנחנו משתמשים במתודה `from_iso8601` ליצירת תאריכים ממחרוזות. את ההשוואה אנחנו מבצעים בעזרת פונקציה `equal`, שמחזירה `False` או `True`.

## ראה גם:
1. [תיעוד `gleam/date_time`](https://hexdocs.pm/gleam_date_time/gleam/date_time.html)
2. [תיעוד `gleam/stdlib`](https://hexdocs.pm/gleam_stdlib/gleam/stdlib.html)
3. [פורום של קהילת Gleam](https://gleam.run/community/)
4. [קורס אונליין בנושא Gleam programming](https://gleam.run/tutorials/)
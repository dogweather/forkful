---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
נשים את תאריך למחרוזת הוא הפעלה שמשנה תאריך למחרוזת, מאפשרת לנו להציג תאריכים בצורה אנושית יותר ובצורה שמתאימה לקורא. זה נחוץ כדי להפוך את התאריך הממאיר למשהו שניתן לחלוק ולאחסן.

## איך בדיוק:
```Gleam
import gleam/dateTime
import gleam/codec
let date = dateTime.from_timestamp(1466835840) // {year = 2016, month = 06, day = 25, hour = 13, minute = 04, second = 00}
let string_date = codec.to_string(date)
```
פלט:
```text
"2016-06-25T13:04:00.000Z"
```

## צלילה עמוקה
תחילה, נשים את תאריך למחרוזת הייתה לא מתוחזקת בצורה טובה. מגבלות התוכנית הקיימת הפכו את התהליך לתמידי. גלים משפר את זה עם פונקציות מובנות שמאפשרות ניפוי קל יותר ותמיכה רחבה יותר בתבניות. ניתן גם להשתמש במודולים נוספים כמו `codec` עבור פונקציונאליות המרה מורחבת.

אבל למה לבחור בנישום למחרוזת? התשובה שוכנת בכך שהמרה למחרוזת מאפשרת את התאומה המרשימה של תאריך עם בסיסי נתונים או פורמטים אחרים, כמו JSON. אסלאי שלכם לכך הוא השמירה על אחידות ושמירה על תבנית נתונים מוכרת.

## ראה גם
- מדריך גלים רשמי: https://gleam.run/book/tour/basics.html
- תיעוד DateTime בגלים: https://hexdocs.pm/gleam_stdlib/gleam/dateTime.html
- תיעוד Codec בגלים: https://hexdocs.pm/gleam_stdlib/gleam/codec.html
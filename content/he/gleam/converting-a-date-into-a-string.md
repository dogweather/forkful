---
title:                "המרת תאריך למחרוזת"
html_title:           "Gleam: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

למה לצרוך להמיר תאריכים למחרוזות? מספרי תאריכים הם חלק נפרד בעולם של פיתוח ופונקציותיהם חשובות לכל אפליקציה שמתייחסת לזמן. המרת תאריך למחרוזת מאפשרת לנצל את היכולות המתקדמות של מחשבים כדי לטפל במגוון רחב של פורמטים והפיכת התאריך לקל להצגה ולעיבוד.

## איך לעשות את זה

במאמר זה נדבר על הדרך להמיר תאריכים למחרוזות בשפת התכנות גלים. כדי לעשות זאת, נשתמש בפונקציות המובנות של השפה ונראה כמה דוגמאות לשימוש בהן.

כדי להמיר תאריך למחרוזת נשתמש בפונקציה `gleam.time.Format.rfc3339` ונעביר לה את התאריך שברצוננו להמיר כפרמטר. כך נקבל את התאריך בפורמט של RFC3339 כמחרוזת.

```Gleam
let my_date = time.build_date(2021, 1, 1)
let date_string = time.Format.rfc3339(my_date)
```

הפלט של משתנה `date_string` יהיה `2021-01-01T00:00:00+00:00` כמו בתמונת מסך של פקודת הטרמינל שנמצאת מתחת.

![שימוש בפונקציה `gleam.time.Format.rfc3339`](https://i.imgur.com/PjXPYz2.png)

בנוסף, נספק דוגמא למרחב פתיחה נוסף של הפונקציה `gleam.time.Format.custom`. במקום לשתמש בפורמט קבוע, נוכל להגדיר את פורמט התאריך לפי הצורך שלנו. בדוגמא הבאה, אנחנו מגדירים פורמט לתאריך שמייצג הודעות במערכת.

```Gleam
let my_date = time.build_date(2021, 1, 1)
let custom_format = time.Format.custom("YYYY/MM/DD [at] hh:mm")
let date_string = time.Format.format(custom_format, my_date)
```

הפלט של משתנה `date_string` יהיה `2021/01/01 at 00:00` כמו בתמונת מסך של פקודת הטרמינ
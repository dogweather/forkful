---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?

הוצאת התאריך הנוכחי בתכנות היא ביצוע של פונקציה שמחזירה את התאריך והשעה הנוכחית. תכניתאים עשויים להשתמש בזה ללוגים, ציוני זמן, ואפשרויות נוספות.

## איך לעשות:

בשפת Gleam, אתמול שהתאריך הנוכחי נגיש אמצעות קוד מסוים:

```gleam
import gleam/otp/time.{Now}
import gleam/int

fn main() {
  let {year, month, day, hour, _, _} = Now.now()
  let formatted_date = int.to_string(year) ++ "-" ++ int.to_string(month) ++ "-" ++ int.to_string(day) ++ " " ++ int.to_string(hour)
  formatted_date
}
```
אם תריצו את הקוד, הפלט יהיה (התאריך ישתנה בהתאם לזמן):

```gleam
2022-2-24 14
```

## מעומק הנושא

הגעת לתאריך הנוכחי היה נושא מעניין בעבר. בתי קודמים, קארנילס השתמשו בהתקן ה"תאריך מן האפוכה" כדי לשמור על התאריך הנוכחי. כמובן זה לא רלוונטי יותר, אך מדוע לדעת.

פלטפורמות ושפות תכנות יש שונות במימוש שלהם אך בגלים, אנחנו משתמשים `gleam/otp/time.Now` לקבלת הזמן הנוכחי.

## ראה גם

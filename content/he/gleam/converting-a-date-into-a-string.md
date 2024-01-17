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

## מה ולמה?

המרת תאריך למחרוזת היא תהליך שבו מתבנקרים תאריך בתוך מחרוזת בפורמט ספציפי. מתוך סיבות שונות, מתכנתים צריכים לבצע פעולה זו לעיתים קרובות.

## איך לעשות?

נותנים ל-gleam את התאריך הרצוי ואת הפורמט הרצוי והמשתנים הנחוצים ומשתמשים בפונקציית המרה למחרוזת, כדי להמיר את התאריך למחרוזת בפורמט ספציפי.

```Gleam
import Time
import Time.Format

let today = Time.now()
let format = "%d-%m-%Y"
let date_string = Time.Format.to_string(today, format)
```

המחרוזת החוצה תכיל את התאריך הנתון בפורמט שנקבע במשתנה format.

פונקציית המרה למחרוזת קיימת למגוון רחב של פורמטים, מה שמאפשר למתכנתים לשלב את התאריך במחרוזת כפי שהם רוצים.

## מכניסה עמוקה

בעבר, כדי להמיר תאריכים למחרוזות, המתכנתים היו נאלצים לבנות תכניות מחשב מורכבות. פונקציית המרה למחרוזת של גליים מאפשרת למתכנתים לבצע את המשימה תוך כדי הופעת עומס נמוך על המחשב.

ישנם תכניות מחשב אחרות שיכולות לבצע פעולה זו, כמו למשל פייתון ו-JavaScript. אך בגליים, הופעת הפונקציה אינה תלויה ביכולות של שפת התכנות הראשונה כלל.

## ראו גם

למידע נוסף על המרת נתונים ב-gleam, הסתכלו על התיעוד הרשמי של השפה.

https://gleam.run/documentation/standard-library/time

למידע נוסף על פונקציות תאריך בשפות אחרות:

https://docs.python.org/3/library/datetime.html

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString
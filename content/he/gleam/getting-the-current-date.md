---
title:                "קבלת התאריך הנוכחי"
html_title:           "Gleam: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

בכל יום אנו נתקלים במספר מצבים שבהם אנו זקוקים לתאריך נוכחי - לדוגמה, כאשר יוצאים להזמדם לחברים או כאשר אנו צריכים להגיש מסמכים למועד קבלתם. תאריך נוכחי כזה יכול להיות כולל טוב גם לשימושים שונים בתוך קוד, כגון לחישוב זמנים או לעשיית בדיקות.

## כיצד לעשות זאת

כדי לקבל את התאריך הנוכחי בשפת גלים, אנו צריכים להשתמש בפונקציה המובנית "Date.now()", בשילוב עם הפונקציה "Date.to_string()" בכדי להציג את התאריך בתבנית שלנו.

```
Gleam
import Date

let current_date = Date.now()
let formatted_date = Date.to_string(current_date, "%d/%m/%Y")

IO.print(formatted_date)
```

התוצאה הנוצרת תהיה תאריך נוכחי בפורמט של יום/חודש/שנה, לדוגמה 25/12/2021.

## חקירה עמוקה

השתמשנו בפונקציה Date.now() ו-Date.to_string() כדי לקבל תאריך נוכחי ולהציגו בתבנית מסוימת, אך ישנן עוד אפשרויות לניהול תאריכים בשפת גלים. למשל, ניתן להשתמש בספריית Date בשילוב עם פונקציות נוספות כדי לבצע פעולות נוספות כגון הוספת ימים או חודשים לתאריך נתון.

## ראה גם

- [מדריך רשמי על פונקציות תאריך בשפת גלים](https://gleam.run/documentation/standard-library/date)
- [דוגמאות לשימוש בפונקציות תאריך בשפת גלים](https://gist.github.com/samuela/2551c4be72853957e29d3194f6d46c33)
- [קהילת הפיתוח של גלים - פורום דיון על שפת גלים ופרויקטים נוספים המתבססים עליה](https://community.gleam.run/)
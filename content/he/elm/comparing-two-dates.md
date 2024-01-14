---
title:                "Elm: להשוות שני תאריכים"
simple_title:         "להשוות שני תאריכים"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה:

יש שלושה סיבות עיקריות למה כדאי להשוות שתי תאריכים בתכנות בשפת Elm:

1. אפשרות לבדיקת תוקף של כרטיס אשראי או אישור תאריך ללוח זמנים.
2. אפשרות לבדיקת תאריכי הגעה ויציאה לטיסה או נסיעת רכב.
3. אפשרות ליצור תאריך חדש על ידי הוספת ימים או חודשים לתאריך קיים.

בעזרת השוואת שתי תאריכים, ניתן ליצור כלים משמעותיים ושימושיים בתוך יישומי Elm.

## איך לעשות זאת:

```Elm
import Time
import Date

date1 = Date.fromCalendarDate 2021 1 1
date2 = Date.fromCalendarDate 2021 12 31

Time.inDays (Date.toTime date2) - Time.inDays (Date.toTime date1)
-- פלט: 364 ימים
```

בדוגמה זו, אנו משווים שתי תאריכים על ידי המרתם למשתני זמן וחישוב הימים הנמצאים ביניהם. כך ניתן לקבל תוצאה של כמה ימים עברו מתאריך אחד לתאריך השני.

למשל, אם נרצה ליצור תאריך חדש של אחרי 30 ימים מהתאריך הנוכחי, ניתן להשתמש בפונקציה `Date.add` כדי להוסיף ימים לתאריך הנוכחי.

```Elm
date = Date.today
newDate = Date.add Date.days 30 date
-- פלט: מחר
```

## מבוא:

השוואת שתי תאריכים כוללת הרבה יצירתיות ואפשרויות בעולם התכנות בשפת Elm. כדי להתעמק יותר בנושא, ניתן ללמוד על פונקציות נוספות כמו `Date.compare` שמשווה את התאריכים לעומק יותר, או `Date.fromString` שמאפשרת ליצור תאריך מתחרטר מתווך מחרוזת.

## ראה גם:

- טורים על השוואת תאריכים ב-Elm  https://dev.to/jessicard/comparing-dates-with-elm-4n5j
- מדריך לפונקציות תאריך ב-Elm  https://elmprogramming.com/guide/date.html
-
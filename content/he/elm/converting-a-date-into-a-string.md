---
title:                "Elm: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מדוע

ממשקי משתמש רבים מציגים תאריך בתור מחרוזת, כך שהמשתמש יוכל לקרוא אותו בצורה נוחה. המרת תאריך למחרוזת היא פעולה שנדרשת בהרבה אפליקציות, ולכן חשוב לדעת איך לבצע אותה בצורה נכונה.

## כיצד לעשות זאת

ניתן להמיר תאריך למחרוזת באמצעות פונקציית `toString` המובנית של אלם. הנה דוגמא לקוד שיתבצע ייצוג של תאריך כמחרוזת בפורמט של "יום-חודש-שנה": 

```elm
import Date exposing (Day(..), Month(..))
import Time exposing (Posix)

dateToString : Posix -> String
dateToString date =
  let
    posixToDate = Date.fromTime date
  in
    Date.toString DD/MM/YYYY posixToDate
```

כאן אנו משתמשים בפונקציות המובנות `Date.fromTime` ו-`Date.toString` כדי להמיר את התאריך לפורמט מבוקש. ניתן להתאים את הפורמט לפי הצורך על ידי שימוש במשתני תאריך וחודש מתאימים מהפונקצייה `Date.toString`.

מתוך הקוד הנ"ל נקבל את התוצאה הבאה:

```elm
dateToString 1618725650 --> "17/04/2021"
```

## Deep Dive

בנוסף לפונקציה `toString` המובנית, ניתן גם להשתמש בפונקציות מתקדמות יותר כגון `Date.fromTime`, `Date.toTime`, ו-`Date.fromCalendarDate` על מנת לשלב פרטים נוספים בתאריך, כגון שעה, דקה ושנייה. ניתן גם להשתמש בפונקציות ייצוג שונות כמו `Date.toIso8601String` לפורמט ISO8601, או `Date.toRfc2822String` לפורמט RFC2822.

כמו כן, חשוב לקרוא את התיעוד של אלם כדי לדעת על אפשרויות נוספות לייצוג תאריכים ומחרוזות.

## ראה גם

- [תיעוד פונקציות היצוג של אלם](https://package.elm-lang.org/packages/elm/core/latest/Date)
- [מדריך לתאריך וזמן באלם](
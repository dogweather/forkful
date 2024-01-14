---
title:                "Elm: המרת תאריך למחרוזת"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה
רובנו רוצים לתצוגה תאריך מסוים כולל התאריך והשעה, אבל באופן טבעי באינטרנט אנחנו משתמשים במחרוזות כדי להציג אותו. כדי לעשות זאת, אנו צריכים להמיר את התאריך לטקסט. במאמר הזה, אנחנו נלמד כיצד לבצע פעולה זו בעזרת שפת Elm.

## איך לעשות זאת
תחילה, נצטרך להשתמש במודול Date שנמצא בתוך ספרייה הסטנטרדית של Elm. לאחר מכן, נוכיח לבצע מספר פעולות כדי להמיר את התאריך למחרוזת.

```elm
import Date exposing (Day, Month, Year, Time)
import Date.Format exposing (format)

today : Date
today =
    Date.fromTime (Time.fromHours 11)

formatToday : String
formatToday =
    format "%d/%m/%Y %H:%M" today
```

קוד זה ייצור את התאריך הנוכחי כמו נעילה בין שני זמני `today` ו-`now`. התאריך יושת במחרוזת הנכונה יותר כמו ״1/12/2020 11:00״. ניתן לשנות את הפורמט לפי הצורך, אך יש לוודא כי השימוש בתבניות המתאימות כדי להציג את התאריך הנכון במחרוזת.

## העמקה
כשאנחנו ממירים את התאריך למחרוזת, האם הוא מתמזג עם תוכן אחר במידה והוא חלק ממחרוזת. הוא ישפיע על התוצאה המוחזרת. ייתכן כי התאריך מופיע בתוך מחרוזת כמו ״Today is the 1st of December״. כדי לשמור על פירוק המחרוזת, אנו צריכים להתחשב גם בתבנים המתאימים כדי להמיר את התאריך בצורה נכונה למחרוזת המתאימה.

## ראה גם
* [תיעוד Date של Elm](https://package.elm-lang.org/packages/elm/time/latest/Time)
* [מדריך מפורט על כיצד להשתמש במבני תבניות](https://package.elm-lang.org/packages/elm/time/latest/Time#Format)
---
title:                "חישוב תאריך בעתיד או בעבר"
date:                  2024-01-20T17:31:34.754919-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא פעולה שבה מוסיפים או מחסירים זמן מתאריך קיים. תכנתים עושים זאת כדי לתזמן אירועים, לקבוע מועדים או ליצור תזכורות.

## איך עושים את זה:
ב-Elm, אנו משתמשים בפונקציות מתוך ספריות זמן, כמו `elm/time` כדי להוסיף ולהחסיר זמן. זה דוגמה של איך לעשות זה:

```Elm
import Time
import Date exposing (Date)
import Date.Extra as Date
import Task

type Msg
    = NewDate Date

-- הוסף ימים לתאריך
addDays : Int -> Date -> Task.Task Time.Error Date
addDays days date =
    Task.succeed date
        |> Task.andThen (Date.add Days days)

-- יצירת פונקציית עזר לחישוב התאריך
calculateFutureDate : Int -> Date -> Cmd Msg
calculateFutureDate numberOfDays fromDate =
    addDays numberOfDays fromDate
        |> Task.attempt NewDate

-- ייצוא דוגמא של הפונקציה באמצעות פקודה (Cmd)
sampleDate : Date
sampleDate = Date.fromIsoString "2023-04-01" |> Result.withDefault (Date.fromTime 0)

-- הפקודה תיצור תאריך חדש עם הוספת 10 ימים
exampleCmd : Cmd Msg
exampleCmd = calculateFutureDate 10 sampleDate
```
פלט דוגמא:
```
NewDate <תאריך חדש: 2023-04-11>
```

## צלילה לעומק
בתחילה ב-Elm, חישובי תאריך לא היו פשוטים. היה צורך בהתמודדות ישירה עם זמן UNIX (משתמש ב-milliseconds). פונקציונליות בעתיד או בעבר הופכת להיות יותר נגישה עם ספריות כמו `elm/time` ו-`Date.Extra`. עבור התאריכים, `Date` מאפשר יצירת תאריכים, בעוד `Date.Extra` מכיל פונקציות נוספות לחישובים מורכבים יותר. כחלופה, אפשר לנקוט בגישת חישוב ידני על ידי עבודה עם מספרים וזמן UNIX, אך זה יכול להיות מסובך ופגיע לשגיאות.

בעת חישוב תאריך, חשוב לזכור לטפל בשעון קיץ ושעון חורף ובאזורי זמן. ספריות אלו לרוב לא עוסקות ישירות באזורי זמן, ולכן מומלץ להשתמש בפונקציות שנותנות תמיכה במקרה שהאפליקציה דורשת כך.

## ראה גם
- מסמכי `elm/time`: https://package.elm-lang.org/packages/elm/time/latest/
- תיעוד ומקורות מידע נוספים על `Date.Extra`: https://package.elm-lang.org/packages/justinmimbs/date-extra/latest/
- עמוד הסבר על זמן UNIX: https://www.unixtimestamp.com/
- אינטראקציה עם אזורי זמן ב-Elm: https://package.elm-lang.org/packages/justinmimbs/timezone-data/latest/

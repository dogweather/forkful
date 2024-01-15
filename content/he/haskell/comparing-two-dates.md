---
title:                "להשוות שני תאריכים"
html_title:           "Haskell: להשוות שני תאריכים"
simple_title:         "להשוות שני תאריכים"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

השוואת שני תאריכים בתכנות בחסקל עשויה להיות רלוונטית כאשר נרצה לבדוק אם התאריכים מתאימים להשוואה, לדוגמה כאשר נרצה לבדוק האם תאריך מסוים נמצא בין שני תאריכים אחרים.

## כיצד לעשות זאת

תחילה, נצטט את המודול Data.Time כדי להשתמש בפונקציות איתור תאריכים של הסטנדרט של חסקל. לדוגמה, נשתמש ב־UTCTime להצגת תאריכים בזמן יוניברסלי. 

```
import Data.Time

-- תאריך התחלה
start :: UTCTime
start = UTCTime (fromGregorian 2021 01 01) (timeOfDayToTime (TimeOfDay 12 0 0))

-- תאריך סיום
end :: UTCTime
end = UTCTime (fromGregorian 2021 01 15) (timeOfDayToTime (TimeOfDay 12 0 0))

-- בדיקה אם תאריך נמצא בין התאריכים הנתונים
checkIfInRange :: UTCTime -> Bool
checkIfInRange date = date >= start && date <= end

main = do
  -- תאריך לבדיקה
  let dateToCheck = UTCTime (fromGregorian 2021 01 10) (timeOfDayToTime (TimeOfDay 12 0 0))
  print $ checkIfInRange dateToCheck

```

פלט:
```
True
```

## חפירה עמוקה

בנוסף להשוואה פשוטה של תאריכים, ניתן לבצע גם השוואות נוספות עם תכנות בחסקל. לדוגמה, ניתן לבדוק האם תאריך מסוים חולף את תקופת ניסיון מסוימת, לבדוק האם תאריך הוא יום חול או שבת, ועוד.

## ראו גם

- [מודול Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [תיעוד חסקל רשמי](https://www.haskell.org/documentation/#libraries)
- [מדריך להתחשבות זמן בחסכול](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_Time_And_Date)
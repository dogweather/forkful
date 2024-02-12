---
title:                "פרסום תאריך ממחרוזת"
aliases:
- /he/haskell/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:49.297945-07:00
model:                 gpt-4-0125-preview
simple_title:         "פרסום תאריך ממחרוזת"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

פיענוח תאריך ממחרוזת בהאסקל כולל המרה של ייצוגים טקסטואליים של תאריכים לפורמט מובנה שהתוכנית יכולה לתפעל. התהליך הזה הוא יסודי ליישומים הטופלים נתונים קלנדריים, אפשר לפונקציות כמו חישוב משכים, תזמון ואימות נתונים.

## איך לעשות:

מחוץ לקופסה, האסקל מציע כלים בסיסיים לפיענוח תאריכים, אולם שימוש בספריות כמו `time` לפונקציונליות הליבה ו`date-parse` או `time-parse` לפיענוח גמיש יותר יכול באופן משמעותי לפשט את המשימה.

ראשית, ודא שהספריה `time` זמינה לך; היא לעיתים קרובות כלולה עם GHC, אך אם אתה צריך לציין אותה כתלות, הוסף את `time` לקובץ cabal של הפרויקט שלך או השתמש ב`cabal install time` כדי להתקין אותה ידנית.

```haskell
import Data.Time.Format
import Data.Time.Clock
import System.Locale (defaultTimeLocale)

-- שימוש בספריית time לפיענוח תאריך בפורמט סטנדרטי
parseBasicDate :: String -> Maybe UTCTime
parseBasicDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" 
```

דוגמה לשימוש ולפלט:

```haskell
main :: IO ()
main = print $ parseBasicDate "2023-04-01"

-- פלט: Just 2023-03-31 22:00:00 UTC
```

לתרחישים מורכבים יותר, בהם אתה צריך להתמודד עם מספר פורמטים או לוקליזציות, ספריות צד שלישי כמו `date-parse` יכולות להיות נוחות יותר:

בהנחה שהוספת את `date-parse` לתלויות שלך והתקנת אותה, הנה איך ייתכן שתשתמש בה:

```haskell
import Data.Time.Calendar
import Text.Date.Parse (parseDate)

-- פיענוח מחרוזת תאריך עם הספריית date-parse תומך במספר פורמטים
parseFlexibleDate :: String -> Maybe Day
parseFlexibleDate = parseDate
```

דוגמה לשימוש עם `date-parse`:

```haskell
main :: IO ()
main = print $ parseFlexibleDate "April 1, 2023"

-- פלט: Just 2023-04-01
```

כל דוגמה מדגימה את הגישה היסודית לקחת מחרוזת ולהופכה לאובייקט תאריך שימושי בהאסקל. הבחירה בין שימוש בפונקציות פנימיות של ספריית `time` לבין בחירה בפתרון של צד שלישי כמו `date-parse` תלויה בצרכים הספציפיים של היישום שלך, כמו טווח הפורמטים שאתה צריך להתמודד איתם.

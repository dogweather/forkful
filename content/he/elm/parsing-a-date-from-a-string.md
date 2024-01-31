---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:36:05.444026-07:00
html_title:           "Arduino: ניתוח תאריך ממחרוזת"
simple_title:         "ניתוח תאריך ממחרוזת"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
לנתח תאריך ממחרוזת זה להמיר טקסט שמייצג תאריך למבנה תאריך שהשפה מבינה. תוכניתנים עושים את זה כדי לעבוד עם תאריכים באופן יעיל ומדויק, לדוגמה, לאחסון נתונים או להשוואות.

## How to: (איך לעשות:)
ב-Elm אתה פועל בסביבה טיפוסית וטהורה, אז אם אתה רוצה לנתח תאריך ממחרוזת, תצטרך חבילה כמו `justinmimbs/date`.

```Elm
import Date
import Date.Extra.Parse exposing (isoDate)

-- פונקציה שמנתחת תאריך ממחרוזת בפורמט ISO 8601
parseDate : String -> Maybe Date.Date
parseDate dateString =
    isoDate dateString
    
-- שימוש בפונקציה
result = parseDate "2023-04-01"

-- הדפסת התוצאה
case result of
    Just date ->
        Date.toIsoString date  -- "2023-04-01"

    Nothing ->
        "Invalid date"
```

תוצאה מדוגמת:
```Elm
"2023-04-01" -- תאריך תקין
"Invalid date" -- מחרוזת לא תקינה
```

## Deep Dive (צלילה עמוקה)
בעבר, ניתוח תאריכים ב-Elm היה מסורבל יותר כיוון שהשפה ממוקדת בטיפוסיות מדויקת ובטיהור. היום יש חבילות כמו `justinmimbs/date` שמקלות על התהליך. חלופות כוללות כתיבת פונקציות ניתוח משלך אבל זה עלול לגרום לשגיאות רבות. ניתוח תאריכים חשוב גם לאינטרנציונליזציה שכן פורמטי תאריך משתנים בין תרבויות.

## See Also (ראה גם)
- Elm package for Dates: [justinmimbs/date](https://package.elm-lang.org/packages/justinmimbs/date/latest/)
- Elm Date.Extra.Parse documentation: [Date.Extra.Parse](https://package.elm-lang.org/packages/justinmimbs/date/latest/Date-Extra-Parse)
- Elm Date type information: [Date](https://package.elm-lang.org/packages/elm/time/latest/Time#Date)

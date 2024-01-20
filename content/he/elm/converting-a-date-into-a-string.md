---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרה של תאריך למחרוזת היא תהליך שבו משנים את המבנה של תאריך ממבנה בינארי (או אובייקט) למחרוזת (במיוחד בפורמט הדרוש). מתכנתים עושים את זה כדי להציג את התאריך בצורה מובנת או כדי לשמור אותו במקום כלשהו.

## איך לגרום לזה לקרות:
כאן נראה דוגמא להמרת תאריך למחרוזת באמצעות Elm:

```Elm
import Time exposing (Posix)
import Time.Extra exposing (toISO8601)

type alias Model =
  { time  : Posix
  , asStr : String
  }

getTimeAsStr : Model -> String
getTimeAsStr model =
  toISO8601 model.time
```
תוצאות האמונה הם:
```Elm
> getTimeAsStr { time = Time.millisToPosix 1617580800000, asStr = "" }
"2021-04-05T00:00:00.000Z" : String
```

## צלילה עמוקה:
(1) בהקשר היסטורי, אלם הפך לשפה פופולרית בקרב מתכנתי קידמה בזכות הערכה הגבוהה של שפה אמינה וחזקה. 

(2) האחריות של המתכנתים היא להחליט איזה פורמט מחרוזת לשקול. הנה כמה אפשרויות: `ISO8601`, `RFC2822`, או `Pretty`. במקרה של פומט ה- `ISO8601`, זה מחזיר מחרוזת בפורמט `"2021-04-05T00:00:00.000Z"`

(3) פעולה זו כלולה בחבילת `Time.Extra`.

## ראה גם:
נא להסתכל במקורות הללו למידע נוסף:

1. הדרכות ומידע נרחב יותר על אלם: [Docs](https://package.elm-lang.org/packages/elm/time/latest/)
2. איך להמיר פורמטים שונים של חותמת זמן מתוך אלם: [Time Formats in Elm](https://discourse.elm-lang.org/t/converting-different-time-stamp-formats/6748)

לא יש כאן "חלק סיכום".
---
title:                "פרסום תאריך ממחרוזת"
aliases: - /he/elm/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:41.416686-07:00
model:                 gpt-4-0125-preview
simple_title:         "פרסום תאריך ממחרוזת"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
הפענוח של תאריך ממחרוזת ב-Elm כולל המרת מידע טקסטואלי המייצג תאריכים ושעות לתצורה ש-Elm יכול להבין ולתפעל, במיוחד לסוג `Date`. תהליך זה קריטי לטיפול בקלט מהמשתמש, הצגת תאריכים באופן ממוקם נכון, וביצוע חישובים הקשורים לתאריכים, ומבטיח שיישומי ה-Elm שלכם יוכלו לעבד מידע זמני בחכמה.

## איך לעשות:
ל-Elm אין יכולות מובנות כמו בחלק מהשפות האחרות לפענוח תאריכים, והוא מסתמך בעיקר על פונקציות מה-Javascript או ספריות לפעולות יותר מורכבות. עם זאת, אתם יכולים להשתמש בחבילת `elm/time` לפענוח בסיסי, ולצרכים יותר מורכבים, הספריה `justinmimbs/date` מומלצת ברחבי.

### פענוח באמצעות `elm/time`:
`elm/time` מספקת את המודול `Time`, שמאפשר לכם לעבוד עם חותמות זמן במקום עם תאריכים קריאים לאדם. אף על פי שהיא לא מפענחת באופן ישיר תאריכים מתוך מחרוזות, אתם יכולים להמיר מחרוזת בתקן ISO 8601 לחותמת זמן POSIX, שאתם יכולים לעבוד איתה.

```elm
import Time exposing (Posix)

-- בהנחה שיש לכם מחרוזת תאריך בתקן ISO 8601
isoDateStr : String
isoDateStr = "2023-01-01T00:00:00Z"

-- המרתה לחותמת זמן POSIX (פונקציה זו מחזירה `Result`)
parsedDate : Result String Posix
parsedDate = Time.fromIsoString8601 isoDateStr

-- פלט לדוגמא: Ok <ערך זמן posix>
```

### פענוח באמצעות `justinmimbs/date`:
לפענוחים מורכבים יותר, כמו עיבוד פורמטים שאינם ISO, ספריית `justinmimbs/date` היא בחירה מוצלחת. הנה איך אתם יכולים להשתמש בה לפרסום מחרוזת תאריך מותאמת אישית:

1. ודאו שהספרייה מותקנת:

```shell
elm install justinmimbs/date
```

2. השתמשו בפונקציה `Date.fromString` לפרסום פורמטי תאריכים מותאמים אישית:

```elm
import Date
import Result exposing (Result(..))

-- נניח שיש לכם מחרוזת תאריך בפורמט מותאם אישית `dd-MM-yyyy`
customDateStr : String
customDateStr = "01-01-2023"

-- פונקציה לפרסום הפורמט המותאם אישית
parseDate : String -> Result String Date.Date
parseDate = Date.fromString "dd-MM-yyyy"

-- דוגמא לשימוש
parsedCustomDate : Result String Date.Date
parsedCustomDate = parseDate customDateStr

-- פלט לדוגמא: Ok (Date.fromCalendarDate 2023 Jan 1)
```

בדוגמאות אלה, הסוג `Result` מכיל או פרסום מוצלח שמניב תאריך (`Ok`) או שגיאה (`Err`), מאפשר לכם לטפל בשגיאות באופן עמיד ביישומי ה-Elm שלכם.

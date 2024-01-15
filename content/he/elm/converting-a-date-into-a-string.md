---
title:                "המרת תאריך למחרוזת"
html_title:           "Elm: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

למה מישהו יבחר להמיר תאריך לסרטים בשפת התכנות אלם?

המרה של תאריך לסרטים עלולה להיות עשויה מטרות שונות, לרבות יצירת טבלאות, פילטרים או רק הצגת תאריך בפורמט שמתאים למשתמש. השימוש בפורמט הנכון בסרטים עוזר למשתמשים להבין את התאריך בצורה יעילה ותקינה.

## איך לעשות זאת באלם

```Elm
import Date

Date.format Date.full
    { year = 2021, month = 11, day = 1 }
-- יצא: "יום ראשון, נובמבר 1, 2021"
```

השימוש בפונקציה `Date.format` מאפשר לנו להמיר תאריך לפורמט שאנחנו מבקשים. ניתן להשתמש בפורמטים שונים, כגון מלא, קצר או מותאם אישית, כדי ליצור את התאריך בצורה שמתאימה למשתמש.

```Elm
import Date exposing (Month(..), MonthFormat(..))

Date.format (Date.custom "MMM d, yyyy")
    { year = 2021, month = 11, day = 1 }
-- יצא: "נוב 1, 2021"
```

ניתן גם להשתמש בפונקציות נוספות, כגון `Date.custom`, כדי לייצר פורמט מותאם אישית לתאריך. ניתן להשתמש בפורמטים של חודשים (MonthFormat) כדי להציג את התאריך בצורה שמתאימה למדינה שלך.

```Elm
import Date exposing (Zone(..), Time)

Date.format (Date.timezone Zone.utc)
    { hour = 13, minute = 30, second = 0, millisecond = 0 }
-- יצא: "13:30:00.000Z"
```

לא יש לנו פורמט תאריך בלבד, אלא גם אפשרות לתצוגת זמן. ניתן להשתמש בפונקציות כגון `Time` כדי להציג את הזמן בפורמט שמתאים למשתמש.

## טעימת עומק

המרה של תאריך לפורמט שאנחנו רוצים עשויה להיות מאתגרת אם אנחנו עובדים עם אתרים שונים ומדינות שונ
---
title:                "פריסת תאריך ממחרוזת"
html_title:           "Haskell: פריסת תאריך ממחרוזת"
simple_title:         "פריסת תאריך ממחרוזת"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
בכדי לשתף בין מחשבים ותוכניות, מתכנתים משתמשים בתאריך כתערך יחיד ומסודר שמייצג את יום מסוים בלוח השנה. לצורך כך, עלינו להמיר תאריך מסוג string לפורמט המתאים לתעריף תאריך. מתכנתים עושים זאת כדי לעבוד עם נתונים מבודדים ויעילים.

## איך לעשות?
באמצעות שפת התכנות הפופולרית Haskell, ניתן לבצע המרה מתאריך מסוג string לפורמט תאריך מתאים תוך שימוש בפונקציות כמו "read" ו- "parseTimeM". לדוגמה, אם נרצה להמיר את התאריך "12/10/2021" לפורמט ISO-8601, נכתוב ב Haskell:

```Haskell
import Data.Time.Format
import Data.Time.Calendar

main = do
  let date = "12/10/2021"
  let parsedDate = readTimeM True defaultTimeLocale "%d/%m/%Y" date :: Maybe Day
  putStrLn (show parsedDate)
```
פלט: Just 2021-10-12

כאן ניתן לראות את השימוש בפונקציות read ו- parseTimeM כדי לבצע המרה ולהציג את התאריך המהודק בפורמט תואם.

## להתעמק עוד:
• בהיסטוריה - בעבר, טכנולוגיות להמרת תאריך כבר היו קיימות, אך בהתאם לצורך העתקה של נתונים בין סביבות שונות, טכנולוגיות חדשות נוצרו כגון פורמט RFC 3339 המשמש להתאמת תאריכים ברשתות מחשבים.

• אלטרנטיבות - בנוסף לשימוש בפונקציות של Haskell, ישנן כמה ספריות שניתן להשתמש בהן למטרת המרת תאריך מסוג string לפורמט תאריך מתאים. כמה מהספריות הפופולריות הן "date" ו- "time" שניתן למצוא ב-Hackage או תוכניות כגון "dateutils" הקיימות במערכת ההפעלה שלך.

• פרטים טכניים - המרת תאריכים ופונקציות ניפוי תאריכים הסומנים במודול Data.Time.Calendar כבר היו קיימים בהתחלה ב-Haskell 98 ונשאבו מביטוי רגולרי חזק בשם "Zeller's congruence". מאז, הוספו גם דטרמיננטים ל- "Julian Day" ופונקציות מיוחדות כגון dayOfWeek ו-isLeapYear.

## ראה גם:
כדי להעמיק עוד בנושא ההמרה של תאריך מסוג string לפורמט תאריך מתאים, ניתן לעיין במקורות המצורפים למאמר זה:

• דוקומנטציית ה-Haskell - https://hackage.haskell.org/package/time-1.11.2/docs/Data-Time-Format.html 

• תעמולת RFC 3339 - https://www.ietf.org/rfc/rfc3339.txt 

• ספרית ה-Haskell עבור הנמצא של ספרות התאריכים - https://hackage.haskell.org/package/time , https://hackage.haskell.org/package/date 

• הנמצא של יישומי dateutils במערכות ההפעלה האהובות - https://www.fresse.org/dateutils/
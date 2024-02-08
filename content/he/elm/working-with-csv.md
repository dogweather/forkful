---
title:                "עובדים עם CSV"
date:                  2024-02-03T19:19:45.139699-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם CSV (ערכים מופרדים בפסיקים) כוללת ניתוח ויצירה של קבצים המאחסנים נתונים טבלאיים בפורמט טקסט פשוט. מתכנתים עושים שימוש נרחב בכך כדי לאפשר החלפת נתונים בקלות בין אפליקציות שונות או לעבד סטי נתונים גדולים באופן יעיל ובטוח מבחינת טיפוסי הנתונים בתוך Elm.

## איך לעשות:

ל-Elm אין תמיכה מובנית לניתוח או ייצור CSV; במקום זאת, חבילות צד שלישי כמו `panosoft/elm-csv` משמשות לעיתים קרובות. הדוגמאות להלן מדגימות את השימוש הבסיסי בספרייה זו לניתוח ויצירת CSV.

### ניתוח CSV

ראשית, יש להוסיף את חבילת ה-CSV לפרויקט ה-Elm שלכם:

```bash
elm install panosoft/elm-csv
```

לאחר מכן, ניתן לנתח מחרוזת CSV לרשימה של רשומות. דוגמא פשוטה:

```elm
import Csv

csvData : String
csvData =
    "name,age\nJohn Doe,30\nJane Smith,25"

parseResult : Result String (List (List String))
parseResult =
    Csv.parse csvData

-- פלט לדוגמה: Ok [["name","age"],["John Doe","30"],["Jane Smith","25"]]
```

### יצירת CSV

כדי ליצור מחרוזת CSV מנתוני Elm, יש להשתמש בפונקציה `Csv.encode`:

```elm
import Csv

records : List (List String)
records =
    [ ["name", "age"]
    , ["John Doe", "30"]
    , ["Jane Smith", "25"]
    ]

csvOutput : String
csvOutput =
    Csv.encode records

-- פלט לדוגמה: "name,age\nJohn Doe,30\nJane Smith,25\n"
```

הגישה הפשוטה הזו מאפשרת לכם לאפשר פונקציונליות של CSV בתוך האפליקציות שלכם ב-Elm, תוך שימוש בסביבה עמידה בטיפוסי נתונים למניפולציה והחלפת נתונים.

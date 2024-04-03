---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:45.139699-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD CSV (\u05E2\u05E8\u05DB\u05D9\
  \u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\u05E4\u05E1\u05D9\u05E7\
  \u05D9\u05DD) \u05DB\u05D5\u05DC\u05DC\u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 \u05D5\
  \u05D9\u05E6\u05D9\u05E8\u05D4 \u05E9\u05DC \u05E7\u05D1\u05E6\u05D9\u05DD \u05D4\
  \u05DE\u05D0\u05D7\u05E1\u05E0\u05D9\u05DD \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05D8\u05D1\u05DC\u05D0\u05D9\u05D9\u05DD \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8\
  \ \u05D8\u05E7\u05E1\u05D8 \u05E4\u05E9\u05D5\u05D8. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05E9\u05D9\u05DE\u05D5\u05E9 \u05E0\
  \u05E8\u05D7\u05D1 \u05D1\u05DB\u05DA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E4\u05E9\
  \u05E8 \u05D4\u05D7\u05DC\u05E4\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.239469-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD CSV (\u05E2\u05E8\u05DB\u05D9\
  \u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\u05E4\u05E1\u05D9\u05E7\
  \u05D9\u05DD) \u05DB\u05D5\u05DC\u05DC\u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 \u05D5\
  \u05D9\u05E6\u05D9\u05E8\u05D4 \u05E9\u05DC \u05E7\u05D1\u05E6\u05D9\u05DD \u05D4\
  \u05DE\u05D0\u05D7\u05E1\u05E0\u05D9\u05DD \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05D8\u05D1\u05DC\u05D0\u05D9\u05D9\u05DD \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8\
  \ \u05D8\u05E7\u05E1\u05D8 \u05E4\u05E9\u05D5\u05D8."
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD CSV"
weight: 37
---

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

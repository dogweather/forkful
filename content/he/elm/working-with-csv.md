---
title:                "עבודה עם קבצי CSV"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם קבצי CSV משמשת לטיפול בנתונים מופרדים בפסיקים, מהווה פורמט פופולרי עבור ייבוא וייצוא של נתונים. תכנתים משתמשים בזה כדי לעבוד עם כמויות גדולות של נתונים בצורה פשוטה ונוחה.

## איך לעשות:
ב-Elm, היכולת לעבוד עם CSV לא באה מובנית. אך, אפשר לעשות זאת בעזרת פונקציות וסיפריות שלישיות. דוגמה:
```Elm
-- פונקציה לפירוק שורה של CSV לרשימת מחרוזות
parseCsvLine : String -> List String
parseCsvLine line =
    String.split "," line

-- לשימוש בדוגמה זו, שמורו את הקוד בקובץ והריצו עם הקלט המתאים
sampleOutput : String -> String
sampleOutput csvData =
    csvData
        |> String.lines
        |> List.map parseCsvLine
        |> Debug.toString

-- הדפסת תוצאה לדוגמה
sampleOutput "name,age\nAlice,30\nBob,25"
```
תוצאת הדוגמה: `"[["name","age"],["Alice","30"],["Bob","25"]]"`.

## עיון מעמיק:
CSV, שהופך לפופולרי בשנות ה-70, הוא פורמט קל לקריאה ולכתיבה גם על ידי אנשים וגם על ידי מחשבים. פורמט JSON מתחרה באוליותיו, עם יכולות מורכבות יותר, אך CSV עדיין ממשיך להיות מועדף במקרים רבים. בעת עבודה עם CSV ב-Elm, חשוב לזכור כי נדרשת טיפול בנתונים טקסטואליים ולעיתים ייבוא של סיפריות נוספות לפירוק והרכבה של הנתונים.

## ראו גם:
- [Elm Guide on JSON](https://guide.elm-lang.org/interop/json.html) - מסביר כיצד לעבוד עם נתוני JSON, פורמט אלטרנטיבי ל-CVS.
- [Elm String documentation](https://package.elm-lang.org/packages/elm/core/latest/String) - מסמכים על טיפול במחרוזות ב-Elm שיכולים לעזור בפיענוח והרכבת CSV.

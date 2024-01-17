---
title:                "עובדים עם קובץ csv"
html_title:           "Haskell: עובדים עם קובץ csv"
simple_title:         "עובדים עם קובץ csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?

CSV הוא פורמט קובץ נפוץ במיוחד בעולם התכנות ומיועד לאחסון נתונים טבלאיים בצורה פשוטה וקריאה. הפורמט מאפשר לנתונים להיחשב בקלות ולהיכנס בתוך תוכניות שונות, כך שמתכנתים יכולים לעבוד איתם ולעבוד עלייהם באופן מהיר ויעיל. 

## איך לעשות?

דוגמאות קוד ניתנות למצוא בתוך המאמר זה, כולן מתוך שפת התכנות Haskell. תוכלו לראות את הקוד הכולל בתוך כוכביות כהערות, על מנת לקלקל את הקוד לתוך השורות הנכונות שנדרשות כדי לעבד נתונים במבנה CSV. הנה כמה דוגמאות קוד ופלט תמונות להדגמה:

```Haskell
-- קליטת קובץ CSV כנתוני מילות מפתח
let csvData = readFile "data.csv"
let keywords = map (takeWhile (/= ',')) (lines csvData)

-- יצירת קובץ CSV מתוך רשימת נתונים טבלאיים
let tableData = [["שם", "גיל"], ["דניאל", "27"], ["שרה", "25"], ["משה", "30"]]
let csvString = unlines $ map (intercalate ",") tableData
writeFile "data.csv" csvString
```

פלט: "שם,גיל"
      "דניאל,27"
      "שרה,25"
      "משה,30"

## מעמקים

CSV הוא אקרונים ל-Comma Separated Values, והטכניקה שמאחורי פורמט הקובץ נוצרה בשנת 1972 ונהפכה למנהג בעולם התכנות. כיום, ישנם תוכניות רבות שמאפשרות עבודה עם CSV ומספקות כלים יעילים למתכנתים. עם זאת, ישנם גם פורמטים אחרים לאחסון נתונים טבלאיים כמו XML ו-JSON. כך שכשמתכנתים יבחרו לעבוד עם CSV, יש להם בדיקה מדוקדקת לעשות מה שתוכניות אחרות לא יעשו.

## ראו גם

למידע נוסף על כיצד לעבוד עם קבצי CSV ב-Haskell, בקרו בקישורים הבאים:

- [Haskell wiki page on Working with CSV files](https://wiki.haskell.org/Working_with_CSV_files)
- [hscsv - A CSV parser and printer for Haskell](https://hackage.haskell.org/package/hscsv)
- [CSV Basics in Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/csv-basics)
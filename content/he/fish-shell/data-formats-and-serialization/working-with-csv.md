---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:35.617357-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9\
  \ CSV (\u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD\
  \ \u05D1\u05E4\u05E1\u05D9\u05E7) \u05DB\u05D5\u05DC\u05DC\u05EA \u05E4\u05D9\u05E2\
  \u05E0\u05D5\u05D7, \u05D8\u05D9\u05E4\u05D5\u05DC \u05D5\u05D9\u05D9\u05E6\u05D5\
  \u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8\
  \ \u05D8\u05D1\u05DC\u05D0\u05D9 \u05E9\u05E0\u05DE\u05E6\u05D0 \u05D1\u05E9\u05D9\
  \u05DE\u05D5\u05E9 \u05E0\u05E8\u05D7\u05D1 \u05DC\u05D4\u05D7\u05DC\u05E4\u05EA\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D9\u05DF \u05D9\u05D9\u05E9\u05D5\
  \u05DE\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\
  \u05E2\u05D9\u05DD \u05D0\u05EA\u2026"
lastmod: '2024-03-13T22:44:40.087235-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9 CSV\
  \ (\u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\
  \u05E4\u05E1\u05D9\u05E7) \u05DB\u05D5\u05DC\u05DC\u05EA \u05E4\u05D9\u05E2\u05E0\
  \u05D5\u05D7, \u05D8\u05D9\u05E4\u05D5\u05DC \u05D5\u05D9\u05D9\u05E6\u05D5\u05E8\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 \u05D8\
  \u05D1\u05DC\u05D0\u05D9 \u05E9\u05E0\u05DE\u05E6\u05D0 \u05D1\u05E9\u05D9\u05DE\
  \u05D5\u05E9 \u05E0\u05E8\u05D7\u05D1 \u05DC\u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D9\u05DF \u05D9\u05D9\u05E9\u05D5\u05DE\
  \u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\
  \u05D9\u05DD \u05D0\u05EA\u2026"
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD CSV"
weight: 37
---

## מה ולמה?

עבודה עם קבצי CSV (ערכים מופרדים בפסיק) כוללת פיענוח, טיפול וייצור נתונים בפורמט טבלאי שנמצא בשימוש נרחב להחלפת נתונים בין יישומים. מתכנתים מבצעים את הפעולות האלו כדי לעבד ולנתח נתונים ביעילות, לאוטומט פעולות, או לשלב עם מערכות אחרות.

## איך לעשות:

Shell Fish בפני עצמו אינו כולל פונקציות מובנות שתוכננו במיוחד לטיפול בCSV. עם זאת, אתם יכולים לנצל כלים של Unix כמו `awk`, `sed`, ו-`cut` לפעולות בסיסיות או להשתמש בכלים מתמחים כמו `csvkit` למשימות מתקדמות יותר.

### לקרוא קובץ CSV ולהדפיס את העמודה הראשונה:
שימוש ב-`cut` כדי להוציא את העמודה הראשונה:
```fish
cut -d ',' -f1 data.csv
```
פלט לדוגמה:
```
Name
Alice
Bob
```

### לסנן שורות CSV בהתבסס על ערך עמודה:
שימוש ב-`awk` כדי למצוא שורות שבעמודה השנייה הערך הוא "42":
```fish
awk -F, '$2 == "42" { print $0 }' data.csv
```
פלט לדוגמה:
```
Bob,42,London
```

### לשנות קובץ CSV (למשל, להוסיף עמודה):
שימוש ב-`awk` להוספת עמודה עם ערך סטטי "NewColumn":
```fish
awk -F, 'BEGIN {OFS=","} {print $0,"NewColumn"}' data.csv > modified.csv
```
פלט לדוגמה ב-`modified.csv`:
```
Name,Age,City,NewColumn
Alice,30,New York,NewColumn
Bob,42,London,NewColumn
```

### שימוש ב-`csvkit` לפעולות מתקדמות יותר:
ראשית, וודאו ש-`csvkit` מותקן. אם לא, התקינו אותו בעזרת pip: `pip install csvkit`.

**המרת קובץ CSV ל-JSON:**
```fish
csvjson data.csv > data.json
```
פלט לדוגמה ב-`data.json`:
```json
[{"Name":"Alice","Age":"30","City":"New York"},{"Name":"Bob","Age":"42","City":"London"}]
```

**סינון עם `csvgrep` של `csvkit`:**
```fish
csvgrep -c 2 -m 42 data.csv
```
הפקודה הזו משחזרת את משימת הסינון אך באמצעות `csvkit`, עם יעד לעמודה 2 לערך "42".

כסיכום, למרות ש-Shell Fish עצמו אולי לא מציע יכולות מובנות לטיפול ישיר בקבצי CSV, האינטגרציה החלקה שלו עם כלים של Unix וזמינותם של כלים כמו `csvkit` מספקים אפשרויות חזקות לעבודה עם קבצי CSV.

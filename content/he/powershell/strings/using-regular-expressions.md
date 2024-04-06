---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:08.417635-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05D1-PowerShell, \u05D0\u05EA\u05D4 \u05D9\
  \u05DB\u05D5\u05DC \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D0\u05D5\u05E4\
  \u05E8\u05D8\u05D5\u05E8\u05D9\u05DD `-match`, `-replace`, \u05D5`-split`, \u05D1\
  \u05D9\u05DF \u05D4\u05D9\u05EA\u05E8, \u05DC\u05D1\u05D9\u05E6\u05D5\u05E2 \u05E4\
  \u05E2\u05D5\u05DC\u05D5\u05EA \u05E2\u05DD \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\
  \u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD. \u05D1\u05D5\u05D0\u05D5\
  \ \u05E0\u05D7\u05E7\u05D5\u05E8 \u05DB\u05DE\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D0\
  \u05D5\u05EA."
lastmod: '2024-04-05T21:53:40.783255-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-PowerShell, \u05D0\u05EA\u05D4 \u05D9\u05DB\u05D5\u05DC \u05DC\u05D4\
  \u05E9\u05EA\u05DE\u05E9 \u05D1\u05D0\u05D5\u05E4\u05E8\u05D8\u05D5\u05E8\u05D9\u05DD\
  \ `-match`, `-replace`, \u05D5`-split`, \u05D1\u05D9\u05DF \u05D4\u05D9\u05EA\u05E8\
  , \u05DC\u05D1\u05D9\u05E6\u05D5\u05E2 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E2\
  \u05DD \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
weight: 11
---

## איך ל:
ב-PowerShell, אתה יכול להשתמש באופרטורים `-match`, `-replace`, ו`-split`, בין היתר, לביצוע פעולות עם ביטויים רגולריים. בואו נחקור כמה דוגמאות:

### שימוש ב`-match` לבדיקה אם מחרוזת תואמת דפוס
האופרטור הזה מחזיר `$true` אם הדפוס נמצא בתוך המחרוזת, ו`$false` אחרת.

```powershell
"hello world" -match "\w+orld"
# פלט: True
```

### חילוץ תוצאות שהתאימו
ניתן לחלץ את הערך שהתאים על ידי גישה למשתנה האוטומטי `$matches`.

```powershell
if ("I have 100 apples" -match "\d+") {
    "Number found: " + $matches[0]
}
# פלט: מספר שנמצא: 100
```

### שימוש ב`-replace` להחלפות
האופרטור `-replace` מחליף את כל המופעים של דפוס עם מחרוזת החלפה מסוימת.

```powershell
"foo bar baz" -replace "ba[rz]", "qux"
# פלט: foo qux qux
```

### פיצול מחרוזות עם `-split`
פצל מחרוזת למערך של תת-מחרוזות בהתבסס על דפוס regex.

```powershell
"The quick-brown_fox jumps" -split "[-_ ]"
# פלט: The quick brown fox jumps
```

### תיאום דפוסים מתקדם
PowerShell תומך גם בפעולות regex מורכבות יותר באמצעות המחלקה `[regex]`, תוך נתינת גישה לשיטות כמו `Matches()`, `Replace()`, ו`Split()`.

```powershell
[regex]::Matches("June 24, August 9, Dec 12", "\b[A-Za-z]+\b").Value
# פלט: June August Dec

[regex]::Replace("100,000", "\B(?=(?:\d{3})+(?!\d))", ",")
# פלט: 100,000

[regex]::Split("one,two;three four", ",|;| ")
# פלט: one two three four
```

דוגמאות אלו מראות את הכוח והגמישות של ביטויים רגולריים ב-PowerShell לניהול נתונים ותיאום דפוסים. באמצעות ניצול regex, מתכנתים יכולים לבצע עיבוד טקסט מורכב ביעילות.

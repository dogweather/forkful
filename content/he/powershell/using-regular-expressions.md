---
title:                "שימוש בביטויים רגולריים"
date:                  2024-02-03T19:18:08.417635-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בביטויים רגולריים"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

ביטויים רגולריים (regex) הם סדרות של תווים המהווים דפוס חיפוש, המשמשים בעיקר לחיפוש וניהול מחרוזות. מתכנתים מנצלים את regex ב-PowerShell למשימות כמו אימות נתונים, ניתוח והמרה בשל יעילותם וגמישותם בטיפול בדפוסים מורכבים.

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

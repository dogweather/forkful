---
date: 2024-01-26 03:46:36.922010-07:00
description: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  \ \u05D4\u05D5\u05D0 \u05E2\u05E0\u05D9\u05D9\u05DF \u05E9\u05DC \u05DB\u05D9\u05D5\
  \u05D5\u05E0\u05D5\u05DF \u05E2\u05E8\u05DA \u05DC\u05E9\u05DC\u05DD \u05D4\u05E7\
  \u05E8\u05D5\u05D1 \u05D1\u05D9\u05D5\u05EA\u05E8 \u05D0\u05D5 \u05DC\u05DE\u05E7\
  \u05D5\u05DD \u05E2\u05E9\u05E8\u05D5\u05E0\u05D9 \u05DE\u05E6\u05D5\u05D9\u05DF\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E2\u05D2\u05DC\u05D9\u05DD\
  \ \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E4\u05D5\
  \u05DA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DC\u05E4\u05E9\u05D5\u05D8\u05D9\
  \u05DD \u05D9\u05D5\u05EA\u05E8, \u05DC\u05E9\u05E4\u05E8 \u05D0\u05EA \u05D4\u05E7\
  \u05E8\u05D9\u05D0\u05D5\u05EA, \u05D0\u05D5\u2026"
lastmod: '2024-03-13T22:44:39.686726-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D4\
  \u05D5\u05D0 \u05E2\u05E0\u05D9\u05D9\u05DF \u05E9\u05DC \u05DB\u05D9\u05D5\u05D5\
  \u05E0\u05D5\u05DF \u05E2\u05E8\u05DA \u05DC\u05E9\u05DC\u05DD \u05D4\u05E7\u05E8\
  \u05D5\u05D1 \u05D1\u05D9\u05D5\u05EA\u05E8 \u05D0\u05D5 \u05DC\u05DE\u05E7\u05D5\
  \u05DD \u05E2\u05E9\u05E8\u05D5\u05E0\u05D9 \u05DE\u05E6\u05D5\u05D9\u05DF."
title: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
weight: 13
---

## מה ולמה?
עיגול מספרים הוא עניין של כיוונון ערך לשלם הקרוב ביותר או למקום עשרוני מצוין. מתכנתים מעגלים מספרים כדי להפוך נתונים לפשוטים יותר, לשפר את הקריאות, או לעמוד בדרישות מתמטיות מסוימות במהלך חישובים.

## איך לעשות:
יש לך מספר כלים נחמדים ושיטות ב-PowerShell לעיגול:

- שיטת `Round()` ממחלקת Math
```PowerShell
[Math]::Round(15.68) # מעגל ל-16
```
- ציון עשרוניים:
```PowerShell
[Math]::Round(15.684, 2) # מעגל ל-15.68
```
- `Ceiling()` ו-`Floor()`, לעיגול תמיד למעלה או למטה:
```PowerShell
[Math]::Ceiling(15.2) # מעגל למעלה ל-16
[Math]::Floor(15.9) # מעגל למטה ל-15
```

## צלילה עמוקה
עיגול מספרים אינו חדש; הוא קיים מזה זמנים עתיקים, שימושי למסחר, מדע ומדידת זמן. כאשר מדובר ב-PowerShell, `[Math]::Round()` משתמשת ב"עיגול הבנקאי" כברירת מחדל, שבו 0.5 עובר למספר הזוגי הקרוב ביותר, מה שמפחית הטיה בפעולות סטטיסטיות.

אתה לא תקוע רק עם שיטות `[Math]`. רוצה שליטה רבה יותר? בדוק את `[System.Math]::Round(Number, Digits, MidpointRounding)` שם אתה יכול לקבוע איך מרכזיות מטופלות: הרחק מאפס או לכיוון הזוגי (כלומר עיגול הבנקאי).

זווית נוספת: האובייקט `System.Globalization.CultureInfo`. הוא עוזר עם עיצוב והעדפות עיגול ספציפיות לאזור כאשר מתמודדים עם מספרים בינלאומיים.

## ראה גם
- המסמכים הרשמיים של מיקרוסופט על שיטות Math: [קישור](https://learn.microsoft.com/en-us/dotnet/api/system.math?view=net-7.0)
- פרטיות עיגול עשרוני ב-.NET: [קישור](https://learn.microsoft.com/en-us/dotnet/api/system.midpointrounding?view=net-7.0)
- דיונים על עיגול ב-StackOverflow: [קישור](https://stackoverflow.com/questions/tagged/rounding+powershell)

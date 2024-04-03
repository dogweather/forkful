---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:48.849128-07:00
description: "\u05D1-PowerShell, \u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\
  \u05E4\u05E8\u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D4\u05D9\u05D0 \u05DE\
  \u05E9\u05D9\u05DE\u05D4 \u05E0\u05E4\u05D5\u05E6\u05D4 \u05E9\u05E2\u05D5\u05D6\
  \u05E8\u05EA \u05DC\u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05DC\u05E7\
  \u05D1\u05DC \u05D4\u05D7\u05DC\u05D8\u05D5\u05EA \u05D1\u05D4\u05EA\u05D1\u05E1\
  \u05E1 \u05E2\u05DC \u05DE\u05D1\u05E0\u05D4 \u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\
  \u05E7\u05D1\u05E6\u05D9\u05DD\u2014\u05DB\u05DE\u05D5 \u05DC\u05DE\u05E0\u05D5\u05E2\
  \ \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D5\u05D9\
  \u05D3\u05D5\u05D0 \u05E9\u05EA\u05D9\u05E7\u05D9\u05D9\u05EA \u05D4\u05D9\u05E2\
  \u05D3\u2026"
lastmod: '2024-03-13T22:44:39.722579-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-PowerShell, \u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\
  \u05E8\u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D4\u05D9\u05D0 \u05DE\u05E9\
  \u05D9\u05DE\u05D4 \u05E0\u05E4\u05D5\u05E6\u05D4 \u05E9\u05E2\u05D5\u05D6\u05E8\
  \u05EA \u05DC\u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05DC\u05E7\u05D1\
  \u05DC \u05D4\u05D7\u05DC\u05D8\u05D5\u05EA \u05D1\u05D4\u05EA\u05D1\u05E1\u05E1\
  \ \u05E2\u05DC \u05DE\u05D1\u05E0\u05D4 \u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\
  \u05D1\u05E6\u05D9\u05DD\u2014\u05DB\u05DE\u05D5 \u05DC\u05DE\u05E0\u05D5\u05E2\
  \ \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D5\u05D9\
  \u05D3\u05D5\u05D0 \u05E9\u05EA\u05D9\u05E7\u05D9\u05D9\u05EA \u05D4\u05D9\u05E2\
  \u05D3 \u05E7\u05D9\u05D9\u05DE\u05EA \u05DC\u05E4\u05E0\u05D9 \u05E0\u05D9\u05E1\
  \u05D9\u05D5\u05DF \u05DC\u05E7\u05E8\u05D5\u05D0 \u05DE\u05DE\u05E0\u05D4 \u05D0\
  \u05D5 \u05DB\u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\u05D4."
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
weight: 20
---

## איך לעשות זאת:
PowerShell מציעה דרך ישירה לבדוק את נוכחות של ספריה באמצעות ה-cmdlet `Test-Path`. ה‏cmdlet הזה מחזיר ערך בוליאני המציין האם הנתיב המצוין קיים. הנה איך אפשר להשתמש בו:

```powershell
# בדיקה אם ספריה קיימת
$directoryPath = "C:\ExamplePath"
$directoryExists = Test-Path -Path $directoryPath
Write-Output "האם הספריה קיימת? $directoryExists"
```

פלט לדוגמה עבור ספריה שקיימת:

```
האם הספריה קיימת? True
```

ועבור ספריה שלא קיימת:

```
האם הספריה קיימת? False
```

עבור סקריפטים יותר מורכבים, במיוחד אלו שמתקשרים עם אחסון ברשתות או בענן, ייתכן שתצטרכו לבדוק נושאים נוספים או פונקציונליות שלא זמינות ישירות דרך `Test-Path`. במקרים כאלה, השימוש במודולים או ספריות של צד שלישי ב-PowerShell יכול להיות מועיל, אף על פי שרוב המשימות השוטפות ניתנות לביצוע עם cmdlets מובנים של PowerShell. לפי העדכון האחרון בידע שלי, לא הייתה ספריה של צד שלישי שהתקבלה על ידי הקהילה באופן נרחב במיוחד לבדיקת קיום של ספריה מעבר למה ש-`Test-Path` מספק, בעיקר מכיוון ש-`Test-Path` בפני עצמו הוא גם עמיד וגם יעיל למטרה זו.

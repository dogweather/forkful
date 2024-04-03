---
date: 2024-01-20 18:02:24.744250-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: ."
lastmod: '2024-03-13T22:44:39.698021-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
weight: 45
---

## איך לעשות:
```PowerShell
# קוד לשליחת בקשת HTTP עם אימות בסיסי
$user = 'myUsername'
$pass = 'myPassword'
$pair = "$($user):$($pass)"
$bytes = [System.Text.Encoding]::ASCII.GetBytes($pair)
$base64 = [System.Convert]::ToBase64String($bytes)
$headers = @{
    Authorization = "Basic $base64"
}

$response = Invoke-RestMethod -Uri 'http://example.com/api/data' -Method Get -Headers $headers

# הדפסת התשובה
$response
```
פלט לדוגמה:
```
id     : 123
name   : דוגמה
status : פעיל
```

## ניתוח עמוק
אימות בסיסי הוא שיטת אימות פשוטה שהחלה להיעשות בשנות ה-90. היא עשויה להיות פחות בטוחה משיטות אימות מודרניות יותר כמו OAuth. בPowerShell, שימוש ב`Invoke-RestMethod` עם הגדרות כותרת מתאימות מאפשרת שליחת בקשות בשיטה זו. לקחת בחשבון שהשימוש באימות בסיסי מחייב חיבור מאובטח (HTTPS) כדי למנוע את הדלפת נתוני האימות.

## ראה גם
- [מידע נוסף על אימות בסיסי ב-HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [מידע על Invoke-RestMethod בתיעוד PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1)

---
date: 2024-01-20 17:44:57.511398-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E4\
  \u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8 \u05D4\u05D9\u05D9\u05EA\u05D4\
  \ \u05D7\u05E9\u05D5\u05D1\u05D4 \u05DB\u05D1\u05E8 \u05DE\u05EA\u05D7\u05D9\u05DC\
  \u05EA \u05D4\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05D6\u05D4 \u05DE\u05D0\
  \u05E4\u05E9\u05E8 \u05DC\u05E0\u05D5 \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05DE\u05E7\
  \u05D5\u05DE\u05D9. Invoke-WebRequest \u05D4\u05D5\u05D0 \u05DB\u05DC\u05D9 \u05D7\
  \u05D6\u05E7 \u05D1-PowerShell, \u05D0\u05D1\u05DC \u05D9\u05E9 \u05D2\u05DD\u2026"
lastmod: '2024-04-05T22:50:53.807713-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E4\u05D9 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8 \u05D4\u05D9\u05D9\u05EA\u05D4 \u05D7\u05E9\u05D5\u05D1\u05D4\
  \ \u05DB\u05D1\u05E8 \u05DE\u05EA\u05D7\u05D9\u05DC\u05EA \u05D4\u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E0\u05D8."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
weight: 42
---

## איך ל:
פשוט, דוגמאות של קוד ופלט:

```PowerShell
# הורדת דף אינטרנט באמצעות Invoke-WebRequest
$response = Invoke-WebRequest -Uri "http://example.com"
# שמירת תוכן הדף לקובץ
$response.Content | Out-File "C:\myfolder\mypage.html"
```

זהו הפלט שלך אחרי הרצה נכונה:
```
<!DOCTYPE html>
<html>
...
</html>
```

## טבילה עמוקה
הורדת דפי אינטרנט הייתה חשובה כבר מתחילת האינטרנט. זה מאפשר לנו לעבוד עם נתונים באופן מקומי. Invoke-WebRequest הוא כלי חזק ב-PowerShell, אבל יש גם כלים אחרים כמו cURL או Wget במערכות אחרות. כאשר אתה משתמש ב-Invoke-WebRequest, זה שולח בקשה HTTP ומקבל תגובה. אפשר לגשת לכותרות, תוכן, ומידע נוסף שבתגובה.

## ראה גם
- [מסמך העזרה של PowerShell ל-Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
- [מקור ReHabliPoint Chain-yurtel](https://httpbin.org) - מאגר לבדיקות בקשות HTTP
- [מדריך ל-Wget](https://www.gnu.org/software/wget/manual/wget.html) - אם אתה רוצה היכרות עם כלי אלטרנטיבי

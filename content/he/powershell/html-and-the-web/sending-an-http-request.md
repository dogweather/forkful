---
date: 2024-01-20 18:00:35.976617-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E9\u05D9\u05DE\
  \u05D5\u05E9 \u05D1-`Invoke-WebRequest` \u05DC\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\
  \u05E7\u05E9\u05D4 GET."
lastmod: '2024-03-13T22:44:39.693426-06:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1-`Invoke-WebRequest` \u05DC\u05E9\u05DC\
  \u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05D4 GET."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
weight: 44
---

## איך לעשות:
שימוש ב-`Invoke-WebRequest` לשליחת בקשה GET:

```PowerShell
$response = Invoke-WebRequest -Uri "http://example.com/api/data"
$response.Content
```

תוצאה:

```
{ "id": 1, "name": "A Thing", "status": "Awesome" }
```

שליחת בקשה POST עם גוף בקשה:

```PowerShell
$body = @{
    id=101
    value="Cool Data"
} | ConvertTo-Json

$response = Invoke-WebRequest -Uri "http://example.com/api/update" -Method Post -Body $body -ContentType "application/json"
$response.StatusCode
```

תוצאה:

```
200
```

פה הקבלת הקוד תשובה שמציין שהבקשה הצליחה.

## עיון מעמיק:
`Invoke-WebRequest` הוא חלק מ-PowerShell מגרסה 3.0 ומעלה, מאפשר שליחת בקשות רשת מסוגים שונים. לפניו, הייתה צורך להשתמש בעטיפות של .NET.
אלטרנטיביות כמו `curl` או `Invoke-RestMethod` (לAPIs) קיימות ומתאימות לתרחישים שונים. בשונה מ-`Invoke-WebRequest`, `Invoke-RestMethod` מפרסר את התשובה לאובייקט פי.אס. מפשט את השימוש בנתונים.

בהיבט של יישום, כששולחים בקשות זה חשוב לטפל בHeaders, HTTP Methods, ולנהל סשנים עם cookies למיניהם כדי להבטיח תקשורת בטוחה ויציבה.

## ראה גם:
- [Invoke-WebRequest Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [Invoke-RestMethod Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [Official PowerShell GitHub Repository](https://github.com/PowerShell/PowerShell)
- [.NET HttpClient Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)

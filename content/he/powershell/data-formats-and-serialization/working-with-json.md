---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:56.790004-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: #."
lastmod: '2024-03-13T22:44:39.733496-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
weight: 38
---

## איך לעשות:


### פיענוח JSON
כדי לקרוא או לפענח JSON ב-PowerShell, ניתן להשתמש ב-cmdlet `ConvertFrom-Json`. בהינתן מחרוזת JSON, cmdlet זה ממיר אותה לאובייקט של PowerShell.

```powershell
$json = '{"name": "John Doe", "age": 30, "city": "New York"}'
$person = $json | ConvertFrom-Json
$person.name
```

פלט לדוגמה:

```
John Doe
```

דוגמה זו מדגימה איך לפענח מחרוזת JSON פשוטה על מנת לגשת לתכונות של האובייקט הנוצר.

### יצירת JSON
כדי ליצור JSON מאובייקט של PowerShell, ניתן להשתמש ב-cmdlet `ConvertTo-Json`. זה שימושי להכנת נתונים לשליחה לשירות רשת או לשמירה בקובץ תצורה.

```powershell
$person = [PSCustomObject]@{
    name = "Jane Doe"
    age = 25
    city = "Los Angeles"
}
$json = $person | ConvertTo-Json
Write-Output $json
```

פלט לדוגמה:

```json
{
    "name":  "Jane Doe",
    "age":  25,
    "city":  "Los Angeles"
}
```

קטע קוד זה יוצר אובייקט של PowerShell ולאחר מכן ממיר אותו למחרוזת JSON.

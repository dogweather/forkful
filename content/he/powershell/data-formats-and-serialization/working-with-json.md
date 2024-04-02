---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:56.790004-07:00
description: "\u05D0\u05D9\u05E0\u05D8\u05D2\u05E8\u05E6\u05D9\u05D9\u05EA PowerShell\
  \ \u05E2\u05DD JSON (\u05E6\u05D5\u05E8\u05EA JavaScript \u05DC\u05D9\u05D9\u05E6\
  \u05D5\u05D2 \u05E2\u05E6\u05DE\u05D9\u05DD) \u05E2\u05D5\u05E1\u05E7\u05EA \u05D1\
  \u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 (\u05E7\u05E8\u05D9\u05D0\u05D4) \u05D5\u05D1\
  \u05D9\u05E6\u05D9\u05E8\u05D4 (\u05DB\u05EA\u05D9\u05D1\u05D4) \u05E9\u05DC \u05E0\
  \u05EA\u05D5\u05E0\u05D9 JSON, \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E0\u05E4\u05D5\
  \u05E5 \u05DC\u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05D1\u05E8\u05E9\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.733496-06:00'
model: gpt-4-0125-preview
summary: "\u05D0\u05D9\u05E0\u05D8\u05D2\u05E8\u05E6\u05D9\u05D9\u05EA PowerShell\
  \ \u05E2\u05DD JSON (\u05E6\u05D5\u05E8\u05EA JavaScript \u05DC\u05D9\u05D9\u05E6\
  \u05D5\u05D2 \u05E2\u05E6\u05DE\u05D9\u05DD) \u05E2\u05D5\u05E1\u05E7\u05EA \u05D1\
  \u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 (\u05E7\u05E8\u05D9\u05D0\u05D4) \u05D5\u05D1\
  \u05D9\u05E6\u05D9\u05E8\u05D4 (\u05DB\u05EA\u05D9\u05D1\u05D4) \u05E9\u05DC \u05E0\
  \u05EA\u05D5\u05E0\u05D9 JSON, \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E0\u05E4\u05D5\
  \u05E5 \u05DC\u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05D1\u05E8\u05E9\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
weight: 38
---

## מה ולמה?

אינטגרציית PowerShell עם JSON (צורת JavaScript לייצוג עצמים) עוסקת בפיענוח (קריאה) וביצירה (כתיבה) של נתוני JSON, פורמט נפוץ להחלפת נתונים ברשת. מתכנתים עובדים עם JSON כדי להתממשק עם ממשקי API ברשת, קבצי תצורה, או לקדם החלפת נתונים בין שפות ופלטפורמות שונות, הודות לאופן קל משקלו וחוסר התלות שלו בשפה מסוימת.

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

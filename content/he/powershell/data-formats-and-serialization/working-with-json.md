---
title:                "עבודה עם JSON"
aliases: - /he/powershell/working-with-json.md
date:                  2024-02-03T19:23:56.790004-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:38:23.292741-07:00
html_title:           "Arduino: ניתוח תאריך ממחרוזת"
simple_title:         "ניתוח תאריך ממחרוזת"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
פיענוח תאריכים ממחרוזות הוא תהליך שבו אנו הופכים טקסט לסוג של אובייקט תאריך. זה עוזר למתכנתים להתמודד עם תאריכים בבקרת פורמט, חישובים ושמירת נתונים באופן יעיל.

## How to (איך לעשות:)
```PowerShell
# דוגמא לפיענוח תאריך ממחרוזת
$DateString = "23/04/2023" # פורמט dd/MM/yyyy
$DateObject = [datetime]::ParseExact($DateString, 'dd/MM/yyyy', $null)
Write-Output $DateObject  # יוצג התאריך כאובייקט תאריך של PowerShell

# פלט
Sunday, April 23, 2023 12:00:00 AM
```

```PowerShell
# כיצד להתמודד עם פורמטים שונים ואזורי זמן
$DateString = "2023-04-23T14:00:00Z" # ISO 8601 format
$CultureInfo = [System.Globalization.CultureInfo]::InvariantCulture
$DateObject = [datetime]::Parse($DateString, $CultureInfo)
Write-Output $DateObject

# פלט
Sunday, April 23, 2023 2:00:00 PM
```

## Deep Dive (צלילה עמוקה)
Parsing תאריכים היה אתגר מאז התחלתו של תכנות. למרות שהקונספט נשאר זהה - להפוך טקסט לאובייקט תאריך - הכלים והשיטות השתנו. PowerShell, לדוגמה, עושה שימוש ב.NET Framework לפיענוח תאריכים.

ישנן אלטרנטיבות כמו `Parse` ו-`TryParse`, אך `ParseExact` מאפשר בקרה מלאה על פורמט הקלט. באזורים שבהם פורמטי תאריך יכולים להיות סובייקטיביים, חשוב להשתמש בפונקציות שיבטיחו קריאה נכונה של הנתונים.

## See Also (ראו גם)
- [תיעוד רשמי של עצמי התאריך של .NET](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)

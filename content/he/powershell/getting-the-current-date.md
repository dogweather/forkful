---
title:                "לקבלת התאריך הנוכחי"
html_title:           "PowerShell: לקבלת התאריך הנוכחי"
simple_title:         "לקבלת התאריך הנוכחי"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
קבלת תאריך נוכחי היא פעולה פופולרית בין מתכנתים באמצעות PowerShell. זהו פעולה שבאמצעותה ניתן לקבל את התאריך הדדי ביום נוכחי, כך שניתן להשתמש בו בתוכניות שונות, למשל כדי לנתח את הנתונים של יומן אירועים או ליצור תאריך תקף לפעולות נוספות.

## כיצד לעשות זאת:
בשביל לקבל את התאריך הנוכחי ניתן להשתמש בפקודות הבאות בפקודה הפתוחה של PowerShell:

```PowerShell
$getdate = Get-Date
Write-Output "The current date is $getdate"
```
הפקודה הראשונה מתייחסת לפונקציית Get-Date שמכילה את התאריך הנוכחי, והשנייה משתמשת בפקודת Write-Output להדפיס את התאריך המופיע בפונקציית Get-Date.

יש גם אפשרות להדפיס את התאריך בפורמט מסוים, למשל כאשר רוצים לקבל את התאריך הנוכחי בפורמט ידוע כדי לשמור על סדר שונה. זאת נעשה באמצעות הפקודה הבאה:

```PowerShell
$getdate = Get-Date -Format "dd/MM/yyyy"
Write-Output "The current date is $getdate"
```
בפורמט זה, התאריך יוצג בתבנית שיוכן בינהם שנים, חודשים וימים בפקודת Get-Date.

## חפירה עמוקה:
מצבנו נתעמק עוד יותר, נתארגן וניתן תמונת רקע הספורית על הקלטת התאריך הנוכחי, על אפשרויות גיבוי ועל יתרונות הפקודה המתקרבים.

בדרך כלל תהליך זה היה נעשה באמצעות מספר פקודות נוספות בשביל לקבל את התאריך הנוכחי המדוייק. אך, בכך שהכנו את פקודת Get-Date, נקבל את התאריך שם בפורמט של גיבוי הנתונים המתאימים לשימוש.

## ראה גם:
למידע נוסף ניתן לבקר באתר הבא: https://docs.microsoft.com/he-il/powershell/module/Microsoft.PowerShell.Utility/Get-Date?view=powershell-7
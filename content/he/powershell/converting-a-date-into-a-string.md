---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

המרת תאריך למחרוזת זהו תהליך שבו משנים את הצורה שבה מייצגים תאריך (אשר באופן טבעי הוא משתנה) לצורה סטטית של מחרוזת. בתכנות זה משמש בעיקר לדוגמה לתיעוד, לשמירה על התאימות בין מערכות, או למשתמש הסופי בממשק המשתמש.


## איך:

הנה קוד דוגמה של חלק מהדרכים להמיר תאריך למחרוזת ב-PowerShell:

```PowerShell
$date = Get-Date
$dateStr1 = $date.ToString() # פורמט ברירת מחדל
$dateStr2 = $date.ToString("yyyy/MM/dd") # פורמט מותאם אישית

Write-Host $dateStr1
Write-Host $dateStr2
```

וצפוי שתראה הפלט כמו:

```PowerShell
01/01/2022 00:00:00
2022/01/01
```

## צלילה עמוקה

(1) PowerShell, שכינויה ".NET Framework", מאפשרת המרה של תאריך למחרוזת מאז הגרסה הראשונה שלה ב-2006.

(2) ישנם שיטות חלופיות ב-PowerShell להמרת תאריך למחרוזת, כולל שילוב של הפונקציות Get-Date ו-Format-Date.

(3) בהינתן הפונקציה ToString(), פונקציית המרה זו מגיעה עם גמישות רבה. אתה יכול להגדיר בעצמך את הפורמט שאליו תהליך ההמרה מתבצע.

## ראה גם

1. [תיעוד המרת מחרוזת ב-Microsoft](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convert-to-date?view=powershell-7.1)
2. [פורמטים מותאמים אישית להמרת תאריך למחרוזת](https://www.gngrninja.com/script-ninja/2016/5/24/powershell-getting-a-handle-on-dates-and-times) 
3. [הבנת מחרוזות בפורמט תאריך ושעה ב-PowerShell](https://ss64.com/ps/syntax-f-operator.html)
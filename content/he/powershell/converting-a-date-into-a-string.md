---
title:                "המרת תאריך למחרוזת"
html_title:           "PowerShell: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך למחרוזת היא פעולה שמאפשרת לקוד להציג תאריך ושעה בפורמט שנוח יותר למשתמש הסופי. מפתחים מבוקשים לבצע פעולה זו בשביל נוחות ופונקציונליות גבוהה יותר.

## כיצד לבצע:
הגבלה מיותרת של תאריך למחרוזת היא מאפשרת בפשטית לחלץ את התאריך המבוקש בפורמט הרצוי באמצעות פקודות מסומנות. הנה דוגמא מינימלית של כיצד לבצע את הפלט:

```PowerShell
(Get-Date).ToString("dd/MM/yyyy HH:mm:ss")
```

תוצאה:

```PowerShell
23/08/2021 14:55:10
```

כמו כן, ניתן להשתמש בפקודה אחת לכל התאריכים בפורמט מבוקש מתוך קובץ:

```PowerShell
Get-Content data.txt | ForEach-Object { [DateTime]::Parse($_) } | ForEach-Object { $_.ToString("dd/MM/yyyy HH:mm:ss") } | Out-File output.txt
```

תוצאה:

```PowerShell
23/08/2021 14:55:10
24/08/2021 15:00:00
25/08/2021 16:27:45
```

## צינורות - תיאור מעמיק:
מרת תאריך למחרוזת הוא פעולה שנמצאת בשימוש כבר מאז תחילת תיכנות המחשבים. השימוש במרת תאריך למחרוזת יכול לחסוך זמן רב כאשר משתמשים בפקודות מרוכזות ומתוחכמות.

אלטרנטיבות למרת תאריך למחרוזת כוללות שימוש בפקודה [Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1) המאפשרת להציג תאריך ושעה בפורמט רגיל של PowerShell ושימוש בפקודה [Format-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/format-date?view=powershell-7.1) המאפשרת להתאים את התאריך לסוג המבוקש.

מימוש המרת תאריך למחרוזת איננו טריוויאלי ויכול להיות מאתגר במקרים מסוימים כגון חישוב זמן מלאי או התמודדות עם פורמטים מיוחדים.

## ראו גם:
* [Get-Date פקודת PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
* [Format-Date פקודת PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/format-date?view=powershell-7.1)
* [תאריך ושעה בפורמט מבוקש עם PowerShell](https://www.petri.com/powershell-format-datetime)
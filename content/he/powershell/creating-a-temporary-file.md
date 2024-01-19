---
title:                "יצירת קובץ זמני"
html_title:           "PowerShell: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא פעולה שמשתמשים כמוני בקוד של PowerShell מבצעים כדי ליצור קובץ שאינו קיים במחשב באופן זמני. פעולה זו נועדה ללאת חבלות מיותרות ולאפשר לנו לעבוד עם קבצים שנמחקים אוטומטית לאחר שהם נמצאים לא נחוצים יותר.

## איך לעשות זאת:
כדי ליצור קובץ זמני ב-PowerShell, יש להשתמש בפקודת `New-TemporaryFile`. לדוגמא, אם נרצה ליצור קובץ זמני בשם "tempFile", נכתוב ב-PowerShell:

```powershell
$tempFile = New-TemporaryFile
```

שימו לב שהפקודה מחזירה את הקובץ הנמצא במשתנה `$tempFile`. ניתן להשתמש בקובץ זה בכל פעם שנרצה על ידי התייחסות לשם המשתנה.

## העומק:
יצירת קובץ זמני אינה טכניקה חדשה ותוכננה בשנות ה-60 כדי לסייע למחשבים הקדחים את העומס שהתייצב מולם. הטכניקה המקוטלת היום נועדה ליצור קבצים זמניים כדי לעזור בתחזוקה וניתוח. סביר להניח שפקודת `New-TemporaryFile` פועלת באופן דומה.

ישנן גם אלטרנטיבות ליצירת קבצים זמניים ב-PowerShell, כגון כתיבה לרשת או יצירת קובץ בפקודת הפקודה הנמצאת בתיקיית הזמניים של המערכת. למעשה, המטרה העיקרית של פקודת `New-TemporaryFile` היא לייעל את תהליך יצירת קבצים זמניים בפקודת הפקודה הנמצאת בתיקיית הזמניים של המערכת.

## ראו גם:
למידע נוסף על יצירת קובץ זמני ב-PowerShell ניתן לקרוא במדריך הרשמי של מייקרוסופט: https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/New-TemporaryFile. אתם יכולים גם לחפש מידע אבנטורי יותר בתיעוד ובתרגום מקצועי על הנושאים השונים הנוגעים ל-PowerShell ולתכנות בכלל. כמו כן, ניתן לעקוב אחרי קהילות תומכות לבניית תוכניות ב-PowerShell באתרי רשת האהובים עלינו בפייסבוק ובטוויטר.
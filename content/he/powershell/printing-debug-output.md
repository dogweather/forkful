---
title:                "הדפסת פלט תיקון שגיאות"
html_title:           "PowerShell: הדפסת פלט תיקון שגיאות"
simple_title:         "הדפסת פלט תיקון שגיאות"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

# מה ולמה?

הדפסת פלט דיבאג היא תוכנית עם שתי ערכות מידע. הראשונה היא להציג מידע מועיל על התוכנית, כגון ערכים של משתנים או הודעות שגיאה. השנייה היא לסייע למפתחים לזהות בעיות בקוד. פלט דיבאג מאפשר למפתחים לראות את הזרימה של התוכנית ולמצוא לוגאיקה שפעלה בבעיות.

# איך לעשות:

```PowerShell
Write-Host "Hello Debug Output!"
```

פקודת Write-Host מדפיסה מחרוזת לתוך הפלט הטקסטואלי. זה משמעותי להדפיס צגומורדי ולא לפלט. פלט נמצא ברף טקסטואלי גבוה, כמו התוכניות המתחזות ועל התוכניות המפסיקות במיוחד. הדבר הראשון שלבם להתחיל את הרישיון הים יכול לשפר זה את הגלריית עצנוי

```PowerShell
function Debug-Print($input) {
    Write-Host "Debug Output: $input"
}

Debug-Print "Hello World"
```

בתוך פונקציה זו אנו משתמשים בפקודת Write-Host עם משתנה כדי לייצר את הפלט שלנו. כאן ניתן להשתמש במילים נוספות כדי לתאר את המידע שאנו רוצים להדפיס, כמו שם משתנה או פשטות של המחרוזת.

# חקירה עמוקה:

המעוניינים שנרים ציוייפ פקודת Write-Debug. טלנואליות הם מאפשרים לנו לאפשר גם לבצר או לחפ פקודת הדבה בפרט, לאחר שפישר לו יחוב פונקציות כמו Write-Host. חפש את פקודת Write-Verbose שיודע מאיפה למתור הפקודה. לפי פקודה Write-Debug מאפשר כל מספרים לציוייף את הדבגות. הדיבגה מפורס מחד סבה להדפיס לאוט חביט

# ראו גם:

- [Microsoft Docs על Write-Debug](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_debug_preference?view=powershell-7.1)
- [Microsoft Docs על Write-Verbose](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/write-verbose?view=powershell-7.1)
- [פקודת Write-Host חלק מקומרוני הדשת](https://powershell.org/2007/08/14/working-with-write-host/)
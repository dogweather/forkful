---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה אל `standard error` (stderr) משמשת לדיווח על שגיאות בתוכנית. זה נעשה כדי להפריד הודעות שגיאה מפלט רגיל, כך שיוכלו להיזרם לקובץ או כלי אחר ללא הפרעה.

## איך לעשות:
```PowerShell
# כתיבת שגיאה פשוטה
Write-Error "הודעת שגיאה כאן"

# כתיבת טקסט ל-stderr ישירות
Write-Host "הודעת שגיאה ישירה" -ForegroundColor Red -BackgroundColor Black 1>&2

# דוגמת פלט
הודעת שגיאה כאן
הודעת שגיאה ישירה
```

## עיון מעמיק
כתיבה ל-standard error ב-PowerShell היא חלק מתקן POSIX, אשר מוגדר משנות ה-60. האלטרנטיבות כוללות כתיבה ללוגים, שימוש במשתני מערכת, ועוד. יש לציין שב-PowerShell, לעיתים נכתבות שגיאות באופן אוטומטי ל-stderr על ידי cmdlets מתאימים, כמו `Write-Error`.

## לקריאה נוספת
- [תיעוד Microsoft ל-Write-Error](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-error)

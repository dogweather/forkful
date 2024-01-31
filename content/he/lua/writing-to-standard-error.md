---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
date:                  2024-01-19
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה ל- standard error (stderr) מאפשרת להפריד בין הודעות שגיאה ולוגים לבין הפלט הרגיל של התוכנית. זה חשוב כדי לאבחן בעיות ולעקוב אחרי כשלים בקוד.

## איך לעשות:
```Lua
-- כתיבה ל stderr בלואה
io.stderr:write("שגיאה: קריאת הקובץ נכשלה!\n")

-- כדי להדפיס עם פורמט
io.stderr:write(string.format("שגיאה %d: קריאת הקובץ נכשלה!\n", 404))
```

הפלט שיוצא:
```
שגיאה: קריאת הקובץ נכשלה!
שגיאה 404: קריאת הקובץ נכשלה!
```

## צלילה עמוקה:
במערכות יוניקס, הפרדה בין stderr ו stdout (הפלט הרגיל) החל מהשנים הראשונות, כדי לאפשר ניתוב של הודעות שגיאה לקובץ או מכשיר אחר. בלואה, `io.stderr` הוא ממשק פשוט לזרם השגיאה הסטנדרטי. אפשרויות חלופיות כוללות יצירת קובץ לוגים נפרד או שימוש בספריות נוספות לניהול לוגים.

## קישורים נלווים:
- המדריך הרשמי ל-I/O בלואה: https://www.lua.org/pil/21.html
- מבט על מערכת הקבצים וה-IO בלואה: https://lua-users.org/wiki/IoLibraryTutorial
- עקרונות ניהול שגיאות בתכנות: https://en.wikipedia.org/wiki/Error_handling

---
title:                "המרת תאריך למחרוזת"
aliases:
- /he/powershell/converting-a-date-into-a-string/
date:                  2024-01-20T17:39:09.670326-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת תאריך למחרוזת"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרה של תאריך למחרוזת היא תהליך שבו משנים את פורמט התאריך לטקסט. תכניתנים עושים את זה כדי לקבוע פורמט תצוגה, לשמור נתונים בבסיסי נתונים או להתאים תאריך לדרישות ממשק משתמש.

## איך לעשות:
המירו תאריך למחרוזת בפאוורשל כך:

```PowerShell
# קבלת התאריך הנוכחי
$תאריך = Get-Date

# המרה למחרוזת בפורמט סטנדרטי
$תאריך_כמחרוזת = $תאריך.ToString()

# המרה למחרוזת עם פורמט מותאם אישית
$תאריך_פורמט_מותאם = $תאריך.ToString("yyyy-MM-dd HH:mm:ss")

# הצגת התוצאות
$תאריך_כמחרוזת
$תאריך_פורמט_מותאם
```

פלט דוגמא:

```
יום שלישי 02 פברואר 2023 12:34:56
2023-02-02 12:34:56
```

## צלילה עמוקה
המרת תאריכים למחרוזות היא חלק חשוב בעבודה עם מערכות - במיוחד כאשר ישנה הצורך לשתף פעולה עם ממשקי משתמש ושרתים שונים שעשויים לקבל פורמטים שונים של תאריכים. בעבר, פורמטים אלה נקבעו על ידי פקדים תקניים והיום יש גמישות רבה יותר בזכות פונקציות כמו `ToString()`.

באופן חלופי, ניתן להשתמש ב-cmdlet `Get-Date` עם פרמטרים שונים ליצירת פורמט מותאם אישית מראש, או להשתמש ב- .NET כדי להגדיר פורמטים סופר-מדויקים או פונקציונליות תרחישית.

לגבי ביצועים, שינוי פורמט תאריך למחרוזת הוא יחסית פשוט ומהיר, אך כאשר הוא נעשה בלולאה על כמות גדולה של נתונים, כדאי לשקול קדימות פורמטים ואופטימיזציה לפי השפה ותרבות של המשתמש.

## ראו גם
- תיעוד הרשמי של `Get-Date`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date
- תיעוד על custom date and time format strings: https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings
- פוסט בנושא פורמטינג תאריכים בפאוורשל: https://devblogs.microsoft.com/scripting/hey-scripting-guy-how-can-i-use-windows-powershell-to-determine-the-date-of-the-last-friday-of-the-year/

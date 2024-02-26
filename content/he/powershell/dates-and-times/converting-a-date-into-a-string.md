---
date: 2024-01-20 17:39:09.670326-07:00
description: "\u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05DC\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\
  \u05D9\u05DA \u05E9\u05D1\u05D5 \u05DE\u05E9\u05E0\u05D9\u05DD \u05D0\u05EA \u05E4\
  \u05D5\u05E8\u05DE\u05D8 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05D8\u05E7\
  \u05E1\u05D8. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05D5\
  \u05E2 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05EA\u05E6\u05D5\u05D2\u05D4, \u05DC\u05E9\
  \u05DE\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D1\u05E1\u05D9\
  \u05E1\u05D9 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05DC\u05D4\u05EA\
  \u05D0\u05D9\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA\u2026"
lastmod: '2024-02-25T18:49:37.956638-07:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\
  \u05DA \u05E9\u05D1\u05D5 \u05DE\u05E9\u05E0\u05D9\u05DD \u05D0\u05EA \u05E4\u05D5\
  \u05E8\u05DE\u05D8 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05D8\u05E7\u05E1\
  \u05D8. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05D5\u05E2\
  \ \u05E4\u05D5\u05E8\u05DE\u05D8 \u05EA\u05E6\u05D5\u05D2\u05D4, \u05DC\u05E9\u05DE\
  \u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D1\u05E1\u05D9\u05E1\
  \u05D9 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05DC\u05D4\u05EA\u05D0\
  \u05D9\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA\u2026"
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
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

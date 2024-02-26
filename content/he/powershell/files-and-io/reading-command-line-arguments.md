---
date: 2024-01-20 17:57:26.491628-07:00
description: "\u05E0\u05E7\u05E8\u05D0 \u05DC\u05E7\u05DC\u05D5\u05D8 \u05DE\u05E9\
  \u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4 \u05DB\u05D9 \u05DC\u05E4\
  \u05E2\u05DE\u05D9\u05DD \u05E0\u05E8\u05E6\u05D4 \u05E9\u05D4\u05E1\u05E7\u05E8\
  \u05D9\u05E4\u05D8 \u05E9\u05DC\u05E0\u05D5 \u05D9\u05D4\u05D9\u05D4 \u05D2\u05DE\
  \u05D9\u05E9 \u05D5\u05D9\u05EA\u05D0\u05D9\u05DD \u05DC\u05DE\u05E9\u05EA\u05DE\
  \u05E9\u05D9\u05DD \u05E9\u05D5\u05E0\u05D9\u05DD. \u05D6\u05D4 \u05D3\u05E8\u05DA\
  \ \u05DC\u05D4\u05EA\u05D0\u05D9\u05DE\u05D4 \u05DC\u05E6\u05E8\u05DB\u05D9\u05DD\
  \ \u05D1\u05DC\u05D9 \u05DC\u05E9\u05E0\u05D5\u05EA \u05E7\u05D5\u05D3."
lastmod: '2024-02-25T18:49:37.963430-07:00'
model: gpt-4-1106-preview
summary: "\u05E0\u05E7\u05E8\u05D0 \u05DC\u05E7\u05DC\u05D5\u05D8 \u05DE\u05E9\u05D5\
  \u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4 \u05DB\u05D9 \u05DC\u05E4\u05E2\
  \u05DE\u05D9\u05DD \u05E0\u05E8\u05E6\u05D4 \u05E9\u05D4\u05E1\u05E7\u05E8\u05D9\
  \u05E4\u05D8 \u05E9\u05DC\u05E0\u05D5 \u05D9\u05D4\u05D9\u05D4 \u05D2\u05DE\u05D9\
  \u05E9 \u05D5\u05D9\u05EA\u05D0\u05D9\u05DD \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05E9\u05D5\u05E0\u05D9\u05DD. \u05D6\u05D4 \u05D3\u05E8\u05DA \u05DC\
  \u05D4\u05EA\u05D0\u05D9\u05DE\u05D4 \u05DC\u05E6\u05E8\u05DB\u05D9\u05DD \u05D1\
  \u05DC\u05D9 \u05DC\u05E9\u05E0\u05D5\u05EA \u05E7\u05D5\u05D3."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
נקרא לקלוט משורת הפקודה כי לפעמים נרצה שהסקריפט שלנו יהיה גמיש ויתאים למשתמשים שונים. זה דרך להתאימה לצרכים בלי לשנות קוד.

## How to: (איך ל:)

כדי לקרוא ערכים משורת הפקודה, נשתמש במשתנה `$args`. זה משתנה מיוחד שמכיל את כל הארגומנטים.

```PowerShell
# דוגמה לסקריפט פשוט הדורש שימוש בארגומנטים
param (
  [String]$name,
  [Int]$age
)

Write-Host "שלום, $name! יש לך $age שנים."

# אם נריץ אותו ככה:
# .\script.ps1 -name דני -age 32
# זה ידפיס:
# שלום, דני! יש לך 32 שנים.
```

ניתן גם לעבוד עם `$args` בלי להגדיר פרמטרים במפורש:

```PowerShell
# דוגמה פשוטה עם $args
Write-Host "ארגומנטים שהתקבלו: $args"

# אם נריץ בשורת הפקודה:
# powershell .\script.ps1 שלום 123
# הפלט יהיה:
# ארגומנטים שהתקבלו: שלום 123
```

## Deep Dive (עומק הנושא)
בימים הראשונים, קלט משורת הפקודה היה חיוני מכיוון שזה היה כל מה שהיה. ב-PowerShell, קריאת ארגומנטים דומה, אבל עם קצת סינטקס יותר עשיר וגמיש. הרבה סקריפטים מדברים עם משתמשים או עם סקריפטים אחרים, וזה מאוד עוזר.

חלופות נפוצות כוללות `Get-Content` לקרוא נתונים מקבצים, או `Read-Host` לקבל קלט במהלך ביצוע הסקריפט. בסופו של דבר, הארגומנטים מהפקודה הם רק רשימה של מחרוזות, ואם יש צורך לעבד אותם, נעשה את זה עם כלים פנימיים של PowerShell.

## See Also (ראו גם)
- [about_Functions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions)
- [about_Automatic_Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables)

---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות היא פונקציה שמשנה את כל האותיות הגדולות במחרוזת לקטנות. מתכנתים עושים זאת לצורך תיאום, או כאשר השוואת מחרוזות חייבת להיות רגישה או לא רגישה לאותיות גדולות. 

## איך ל:
אפשר להמיר מחרודת לאותיות קטנות ב-PowerShell עם הפקודה `ToLower()`. דוגמה:

```PowerShell
# הגדרת מחרוזת
$s = "Hello, PowerShell!"
# המרה לאותיות קטנות
$t = $s.ToLower()
write-output $t
```
הפלט יהיה:

```PowerShell
hello, powershell!
```

## עיון מעמיק
1) בהקשר ההיסטורי, הפונקציה `ToLower()` הייתה קיימת כבר ב.NET Framework והועברה ל-PowerShell.
2) אלטרנטיבות: אפשר גם להשתמש בפונקציה `ToLowerInvariant()`.
3) פרטים על המימוש: `ToLower()` משנה את התווים במחרוזת משפה משנה ממחרוזת למחרוזת באותו האורך.

## ראו גם:
- [מסמכי Microsoft על ToLower()](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netframework-4.8)
- [מדריך PowerShell מקיף](https://he.learnpowershell.net/)
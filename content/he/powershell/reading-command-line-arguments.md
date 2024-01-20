---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?

קריאת ארגומנטים של שורת הפקודה היא דרך בה משתמשים יכולים להעביר מידע לסקריפט כאשר הם מריצים אותו. אנו עושים את זה כדי להפוך את הסקריפט ליותר גמיש ומותאם אישית.

## איך לעשות:

```PowerShell
# Code
param (
    [Parameter(Mandatory=$true)][string]$name,
)
Write-Output "שלום $name!"
```
כתוב והרץ את הסקריפט הנל, והגש את `שם` כארגומנט.
 
```PowerShell
# Output
.\myscript.ps1 -name "דני"
"שלום דני!" 
```
זה מדפיס "שלום דני!".


## צלילה עמוקה:

1.היסטוריה: שימוש בארגומנטים בשורת הפקודה התחיל בתקופת המחשוב המוקדמת, כאשר הממשק המנשק היה הדרך היחידה לתקשר עם המחשב.

2.חלופות: אפשרות אחרת להעביר מידע לסקריפט היא באמצעות הקרנה,
אך שימוש בארגומנטים בשורת הפקודה עדיין הפרקטיקה הנפוצה.

3.פרטי ביצוע: אם אתה נותן את `-name` כארגומנט, PowerShell מסתכל על הparam block ומחפש משתנה עם אותו שם. אם הוא מוצא, הוא מכין את הערך הנתון למשתנה.

## ראה גם:

1. [החלטה בין Param ו- $ args ב-PowerShell]
(https://www.faqforge.com/powershell/deciding-between-param-and-args-in-powershell/)

2. [עבודה עם משתנים ב-PowerShell]
(https://www.red-gate.com/simple-talk/sysadmin/powershell/working-with-variables-in-powershell/)

3. [עבודה עם System.Object משתנים ב-PowerShell]
(https://www.red-gate.com/simple-talk/sysadmin/powershell/powershell-day-to-day-admin-tasks-working-with-object-variables/)</p>
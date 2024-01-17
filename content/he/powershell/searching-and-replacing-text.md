---
title:                "חיפוש והחלפת טקסט"
html_title:           "PowerShell: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?

חיפוש והחלפת טקסט היא תהליך שבו מחפשים למחרוזת טקסט ספציפית ומחליפים אותה בתוכן אחר. פעולה זו חשובה למתכנתים מכיוון שהיא מאפשרת להתאים טקסטים לצרכים מסוימים או לתקן באגים בקוד.

## איך לעשות?

תחת תחום זה יש לתת דוגמאות קוד ותוצאות בפורמט ```PowerShell...``` 
קופסאות.

למשל, כדי לחפש ולהחליף סטרינג בקובץ מסוים יש להשתמש בפקודות הבאות:

```PowerShell
Get-Content -Path "path/to/file.txt" | ForEach-Object {$_ -replace "מחרוזת לחיפוש", "מחרוזת להחלפה"} | Set-Content -Path "path/to/file.txt"
```

פקודות אלו מאפשרות לנו לקרוא את קובץ הטקסט, לעבור על כל שורה בו ולבצע החלפה של מחרוזות מסוימות. כמו כן, אנו משתמשים בפקודת "Set-Content" כדי לשמור את השינויים בקובץ.

## חפש עמוק

בתחום זה ניתן ליצור מספר שיטות לחיפוש והחלפה טקסטואליים בסביבת PowerShell. ניתן להשתמש גם בתוכנות חיצוניות לשם יעילות וקריאות יותר.

ניתן למצוא את הפקודות החיוניות בכתב העת "PowerShell Magazine" או באתר הרשמי של PowerShell.

## ראה גם

למידע נוסף ניתן לבקר באתר הרשמי של Microsoft ולהתנסות בפעולת החיפוש והחלפה בסביבת PowerShell.

* [Microsoft חיפוש והחלפת טקסט עם שימוש ב-PowerShell] (https://docs.microsoft.com/en-us/powershell/scripting/samples/searching-and-replacing-text-with-windows-powershell)
* [כתבי העת "PowerShell Magazine"] (https://powershellmagazine.com/)
* [האתר הרשמי של PowerShell] (https://docs.microsoft.com/en-us/powershell/)
---
title:                "עבודה עם YAML"
html_title:           "PowerShell: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

לעבוד עם YAML היא פעולה שחשובה מאוד למתכנתים. יחד עם זאת היא בסיסית ופשוטה לשימוש. YAML היא אקרונים ל"רשימות בסיסיות מסודרות", היא מסייעת לנו לפתור בעיות קשות כגון כתיבת קוד מחדש ושינויים בהגדרות הנדרשות לפרויקטים מסוימים. מתכנתים משתמשים ב YAML לשם כתיבה של מידע מורכב בצורה קריאה וברורה, זה מאפשר להם להיות זמינים למעקב אחר המידע ולשמור על סדר בפרויקטים.

## איך לעבוד עם זה?

קוד ודוגמאות שמציגים איך לעבוד עם YAML בפווורשל בתוך בלוקי קוד ```PowerShell ... ``` 

קישורי קידוף:
- לשלוח נתונים לקובץ YAML צריך להשתמש בפקודת ```Out-Yaml```: 
```
Get-Process | Select-Object Name, ID, CPU | Out-Yaml C:\Users\Admin\Desktop\processes.yaml
```

- לקריאת נתונים מקובץ YAML ניתן להשתמש בפקודת ```ConvertFrom-Yaml```:
```
Get-Content -Path C:\Users\Admin\Desktop\processes.yaml | ConvertFrom-Yaml
```

## מסורת עמוקה

 YAML פותחה כדי להיות פורמט לנתונים מבוסס מסנן. היום היא מיושמת כרכיב סטנדרטי ברוב תכניות הקוד של בינתיים שונות כדי לאחסן מידע מורכב ההותאם בצורה קלילה.

אפשר למצוא מספר אלטרנטיבות ל ימל בתוכניות שונות תכניות שונות, בינהם נפוץ שלהם הוא XML למעשה.

YAML מאפשר למתכנת לעבד נתונים בפורמט אנושי וקריא, וזה ניתן לפתור בעיות המתמשרמות לעבודה כדי ליצור יותר נוח של נתונים ׏מורכבים.

## ראה גם

למידע נוסף על YAML בפואוורשל, ניתן להציץ באתר המפתחים של Microsoft בנושא - https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertfrom-yaml?view=powershell-6
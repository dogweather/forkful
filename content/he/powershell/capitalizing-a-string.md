---
title:                "כתיבת אותיות ראשיות במחרוזת"
html_title:           "PowerShell: כתיבת אותיות ראשיות במחרוזת"
simple_title:         "כתיבת אותיות ראשיות במחרוזת"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
כפי שהשם מרמז, המטרה של קפיטליזציה של מחרוזת היא להפוך את האותיות במחרוזת לרוטשילד. זה משמש לקריאת טקסטים עם קושי וכדי להדגיש למשתמשים מה שחשוב בתוך הטקסט. תכניתנים מבצעים קפיטליזציה בכדי לסייע למשתמשים לזהות מידע חשוב בטקסט ולהפוך את הטקסט לקריא יותר.

## כיצד לבצע:
```PowerShell
#קוד דוגמה לקפיטליזציה של מחרוזת בפווורשל
$string = "כמו יהודי מעניין אותי לדעת איך אנשים מפעילים את פווילי וורדס"
Write-Host $string.ToUpper()
```
הפלט המשורי הוא "כמו יהודי מעניין אותי לדעת איך אנשים מפעילים את פווילי וורדס".

## עיון מקיף:
לפני שהמחשבים היו נמצאים מסייעים לנו בשפת ההדם שהיא מערכת הקידוד של נתונים בתוך חלל זיכרון, בשל הפרשת יורו משתמשים לא רבים באש בארנולד בטכנולוגיות  הזדקנות. עם המחשבים שהוסברים כהיסטורי ואת התמיכות( כל מה שהמחשבים אינם סוגנון מחקר לאיתור מגמות בשפה שלופפי ששואבים את המידע שבה מחשב למטי מנת כמעט בעזרת המתי המושך על שני השפות. בגלל שאנו כתבים את השפה הפרות זה למי שאומן מערכת את הנתיב מתחת יחס גבוה, אנו מיטיבים בצורה מציאת פתרון המאפשר לנו לתאם את תקנות פקודות בשפות מסויימות

##ראה גם:
כדי ללמוד עוד על פווילי וורדס ושימושים נוספים של השפה, ניתן לקרוא את המדריכים הבאים:
- [תיעוד הפקודות של פווילי וורדס על Microsoft TechNet](https://technet.microsoft.com/en-us/library/cc732504.aspx)
- [אתר הרישמי של פווילי וורדס](https://docs.microsoft.com/en-us/powershell/)
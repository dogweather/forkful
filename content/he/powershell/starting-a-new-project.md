---
title:                "התחלת פרויקט חדש"
html_title:           "Clojure: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה? (What & Why?)

התחלת פרויקט חדש היא הפרוצדורה שבה מתכנת נותן למהלך תכנות רענון. זה מאפשר למתכנת ליצור קונספטים חדשים, להמיס בעיות ולהרחיב את הידע שלו.

## איך מבצעים? (How to:)

הנה דוגמה לצורת הפעולה של הוצאת פרויקט חדש לפועל באמצעות PowerShel‪l. 
בקוד הבא אנחנו מלאים את הקונפיגורציה לפקודות שנבצע בהמשך.

```PowerShell
#הגדרת משתנים
$NewProjectDir = 'c:\PsNewProject'
$NewProjectFile = 'main.ps1'

#יצירת התיקיה לפרויקט חדש
New-Item -ItemType Directory -Path $NewProjectDir

#יצירת קובץ חדש עבור הפרויקט
New-Item -ItemType File -Path "$NewProjectDir\$NewProjectFile"
```

על הקוד מעלה ליצור תיקיית פרויקט חדשה עם קובץ `main.ps1` בתוכה.

## צלילה מעמיקה (Deep Dive)

PowerShell הוא שפה אידיאלית למחשבים בסביבת Windows ומגיע עם מערכת ההפעלה. בנוסף ליכולת להתארגן במהירות עם תרשימים, קבצים, ומסדי נתונים, הוא מאפשר שליטה מלאה בניהול מערכת ההפעלה.

בראשיתו, PowerShell השתמש בסיפורי Scripting סטטיים בלבד. אך PowerShell כיום מתקבלת ככלי כדי התחלת פרויקטים חדשים. אתה יכול גם להשתמש ב-PowerShell בעזרת המערכתות הקיימות כמו Python או Node.js

כמו כל שפה אחרת, יש את הארגונומיה של שפת PowerShell. קובץ של PowerShell הוא קובץ `ps1` שהוא מסוג טקסט פשוט. השפה מאפשרת גם השלמות קוד מדהים, האם גורמת לחסכון בזמן של מתכנתים.

## ראו גם (See Also)

- [שפת PowerShell באתר Docs של Microsoft](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1)
- [מדריכים מקוונים בנושא PowerShell](https://www.stackoverflow.com/questions/tagged/powershell)
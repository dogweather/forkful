---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Java: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# מאמר PowerShell: בדיקה אם מדריך קיים
אני מניח שאתם מכירים את ה-cmdlet Test-Path של PowerShell שמאפשר לכם לבדוק אם נתיב קיים. אם אתם לא, אז יש לכם מזל, הגעתם למקום הנכון!

## מה ולמה?
הפקודות של PowerShell המבדיקות אם ספריה קיימת הן קריטיות בקוד שמנהל מערכת קבצים. הן מסייעות נגד ארורי קוד פושע ושגיאות קיבוץ.

## איך לעשות:
Programmer אם אתם רוצים לבדוק אם מדריך מסוים קיים, תשתמשו בפקודה Test-Path של PowerShell. להלן דוגמא:
```PowerShell
PS C:\> Test-Path -Path "your\directory\path"
```
אם המדריך קיים, הקוד יחזיר "True". אם המדריך לא קיים, הקוד יחזיר "False".

## צלילה עמוקה:
בעבר, למתכנתים הייתה התמצאות באמצעים מגוונים עשויים בצורך לבנות את הפקודה שלהם בעצמם. חשבו על התחנה ב-\_ \ _ \ _ \ _. אך בעם הזמן, Microsoft פיתחה את cmdlet Test-Path של PowerShell כדי לספק למתכנתים כלי אמינה לבדיקה של המידע הזה.

מרגע ש- Test-Path קיימת, אלטרנטיבות אחרות נחשבת לפחות אמינות וחולשות במובנים אחרים אם נהוג להתמקד ב- Test-Path לבדיקה אם ספריה קיימת.

מבחינת פרטי האמיתה, \Test-Path עובד באמצעות שמירה בדגמה המסוימת של העץ התיקייה שנמסרה לה ובדיקה אם יש להיא מסלול מתאים קיים.

## ראה גם:
[עזרה ב-Cmdlet Test-Path של PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path?view=powershell-7)
[מדריך PowerShell Beginners](https://www.microsoft.com/en-us/download/details.aspx?id=59185)
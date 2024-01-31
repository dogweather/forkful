---
title:                "בדיקה האם תיקייה קיימת"
date:                  2024-01-20T14:58:24.795119-07:00
simple_title:         "בדיקה האם תיקייה קיימת"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם תיקייה קיימת ב-PowerShell היא אופן למנוע שגיאות יוזם פעולות עם תיקיות. תכניתנים עושים את זה כדי להבטיח תקינות זרימת העבודה ולמנוע נסיונות לגשת לתיקיות שלא קיימות.

## איך לעשות:
כדי לבדוק אם תיקייה קיימת, השתמשו בפקודת `Test-Path` והעבירו את הנתיב של התיקייה שאתם רוצים לבדוק:

```PowerShell
# בדיקה אם תיקייה קיימת
$directoryPath = "C:\Users\ExampleFolder"
$doesExist = Test-Path $directoryPath

# הדפסת התוצאה
if ($doesExist) {
    Write-Host "התיקייה קיימת."
} else {
    Write-Host "התיקייה אינה קיימת."
}
```

תוצאת הדוגמא:
```
התיקייה אינה קיימת.
```

או לקבל תוצאה בוליאנית (True/False) ישר למסוף:
```PowerShell
Test-Path $directoryPath
```

## טבילה עמוקה:
'`Test-Path` היא פקודה שנכנסה לשימוש ב-PowerShell 1.0. זו אחת מפקודות היסוד שסופקו עם השקתה של פלטפורמת PowerShell. יש אלטרנטיבות כמו כתיבת סקריפט ב. NET ושימוש במחלקות הקיימות, אבל `Test-Path` היא הדרך הכי ישירה ופשוטה לבדוק את תקינות הנתיב.

כשמשתמשים ב-`Test-Path`, ניתן להוסיף פלאגים שונים כדי לבצע בדיקות נוספות כמו בדיקה אם הנתיב הוא תיקייה ולא קובץ עם `-PathType`. זה חשוב כאשר אתם רוצים לוודא שאתם מחברים לתיקייה ולא לקובץ בטעות.

כמו כן, שימו לב שבחלונות יש רגישות לאותיות רישיות וקטנות, אבל במערכות פעולה אחרות כמו Linux - ייתכן שתצטרכו להיות מדויקים יותר בנתיבים שלכם.

## ראה גם:
- [about_Test-Path](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path?view=powershell-7.1)
- [Microsoft PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/)

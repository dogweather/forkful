---
title:                "התחלת פרויקט חדש"
aliases: - /he/powershell/starting-a-new-project.md
date:                  2024-01-20T18:04:59.675138-07:00
model:                 gpt-4-1106-preview
simple_title:         "התחלת פרויקט חדש"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?
התחלת פרויקט חדש ב-PowerShell היא הקמת סביבת עבודה לקוד שבו אתה עומד לתרום. מתכנתים עושים זאת כדי לייעל את תהליך הפיתוח, להפריד בין פרויקטים ולארגן את קוד המקור שלהם.

## איך לעשות:
הנה דוגמא פשוטה ליצירת סקריפט חדש:

```PowerShell
# יצירת ספרייה חדשה לפרויקט
New-Item -ItemType Directory -Path "C:\MyProjects\ProjectA"

# יצירת קובץ סקריפט ראשון
New-Item -ItemType File -Path "C:\MyProjects\ProjectA\start.ps1"

# פתיחת הקובץ לעריכה ב VSCode
code C:\MyProjects\ProjectA\start.ps1
```

והנה מה שתקבל:

```PowerShell
    Directory: C:\MyProjects

Mode                 LastWriteTime         Length Name
----                 -------------         ------ ----
d-----          3/1/2023   9:00 AM                ProjectA
```

## עיון נוסף:
בסיס הנתונים של PowerShell נקרא "cmdlets". כל קוד התחלה, כמו `New-Item`, הוא חלק מהאוסף הזה. בעבר, התחלת פרויקט נעשתה ידנית או בעזרת מעטפת פשוטה כמו באַטצ' של חלונות. היום יש לנו את PowerShell, שמאפשרת יצירה וניהול של פרויקטים בצורה יעילה ומאורגנת. לעומת זאת, בעולמות אחרים, כמו Linux, יש כלים דומים כמו bash או shell scripts, אבל PowerShell מתהדרת בכלים אינטגרטיביים לתכנונים מורכבים במיוחד למשתמשי Windows.

## ראה גם:
- [מידע נוסף על cmdlets ב-PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1)
- [הפניות והקשרים לכלים דומים במערכות הפעלה אחרות](https://linux.die.net/man/)
- [מדריך ליצירת סקריפטים ב-PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-arrays?view=powershell-7.1)

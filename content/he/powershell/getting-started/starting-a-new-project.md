---
date: 2024-01-20 18:04:59.675138-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05E0\u05D4\
  \ \u05D3\u05D5\u05D2\u05DE\u05D0 \u05E4\u05E9\u05D5\u05D8\u05D4 \u05DC\u05D9\u05E6\
  \u05D9\u05E8\u05EA \u05E1\u05E7\u05E8\u05D9\u05E4\u05D8 \u05D7\u05D3\u05E9."
lastmod: '2024-03-13T22:44:39.699636-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D0 \u05E4\u05E9\u05D5\u05D8\
  \u05D4 \u05DC\u05D9\u05E6\u05D9\u05E8\u05EA \u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\
  \ \u05D7\u05D3\u05E9."
title: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9"
weight: 1
---

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

---
title:                "שימוש בדיבאגר"
aliases:
- /he/powershell/using-a-debugger.md
date:                  2024-01-26T04:10:14.430082-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בדיבאגר"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/using-a-debugger.md"
---

{{< edit_this_page >}}

## מה ולמה?
שימוש בדיבאגר משמעותו הגדרת נקודות עצירה, שליחת הקוד צעד אחר צעד, צפייה במשתנים, ובדיקת מצב התוכנית שלך בזמן שהיא פועלת. זה משנה את המשחק עבור מתכנתים כי זה מדייק את הבאגים ועוזר לנו להבין מה באמת קורה בקוד שלנו.

## איך לעשות זאת:
ב-PowerShell, אתה יכול לדיבג סקריפטים באמצעות סביבת הכתיבה המשולבת של PowerShell (ISE) או Visual Studio Code (VS Code) עם ההרחבה ל-PowerShell. הנה איך להשתמש בנקודות עצירה בשניהם:

### PowerShell ISE:
```PowerShell
# הגדרת נקודת עצירה בשורה מסויימת
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# הרץ את הסקריפט שלך באופן רגיל
.\MyScript.ps1

# כאשר הסקריפט מגיע לנקודת העצירה, תוכל לבדוק משתנים
$myVariable

# המשך בביצוע
Continue
```

### Visual Studio Code:
```PowerShell
# פתח את סקריפט ה-PowerShell שלך ב-VS Code.
# לחץ ליד המספר השורה כדי להגדיר נקודת עצירה.
# התחל לדבג על ידי לחיצה על F5 או לחיצה על 'התחל דיבאג'.

# VS Code יעצור את הביצוע בנקודת העצירה שלך.
# השתמש בחלונית הדיבאג כדי לצפות במשתנים, לבדוק את מחסנית הקריאה, ולשלוט בזרימה.
```

הדיבאג בשתי הסביבות מאפשר לך לעשות צעד פנימה (F11), צעד החוצה (F10), וצעד החוצה (Shift+F11) בזמן הדיבאג.

## צלילה עמוקה
מבחינה היסטורית, דיבאג ב-PowerShell היה קצת נוקשה; זה דרש הרבה שורות של `Write-Host` כדי להוציא מצבי משתנים או את השיטה הקלאסית של ניסוי וטעייה. עם הופעת PowerShell ISE, ובזמן האחרון יותר, VS Code עם התכונות העשירות שלו לדיבאג, הדיבאג ב-PowerShell הפך כמעט כמו בשפות תכנות מלאות.

אלטרנטיבות לכלים המקוריים לדיבאג של PowerShell כוללים כלים של צד שלישי כמו PowerGUI או שימוש בסביבות פיתוח מובנות כמו Visual Studio עם תוסף ל-PowerShell.

כאשר מיישמים דיבאגר, שקול את הטווח של הסקריפט, במיוחד כאשר עובדים עם סקריפטים שנקראים בעזרת dot-source או מודולים. נקודות עצירה יכולות להיות מבוססות על תנאי, שינוי במשתנים, או בהיותן מבוססות על שורה, מה שמאפשר שליטה מדויקת במהלך פגישת דיבאג.

בנוסף, עם המעבר ל-PowerShell Core (PowerShell רוחב פלטפורמת), הדיבאג בעיקר עבר לידי VS Code, שמספק חוויה עקבית בפלטפורמות שונות.

## ראה גם
למידע נוסף על דיבאג ב-PowerShell:
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)

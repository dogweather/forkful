---
title:                "עיגול מספרים"
aliases:
- he/powershell/rounding-numbers.md
date:                  2024-01-26T03:46:36.922010-07:00
model:                 gpt-4-0125-preview
simple_title:         "עיגול מספרים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/rounding-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
עיגול מספרים הוא עניין של כיוונון ערך לשלם הקרוב ביותר או למקום עשרוני מצוין. מתכנתים מעגלים מספרים כדי להפוך נתונים לפשוטים יותר, לשפר את הקריאות, או לעמוד בדרישות מתמטיות מסוימות במהלך חישובים.

## איך לעשות:
יש לך מספר כלים נחמדים ושיטות ב-PowerShell לעיגול:

- שיטת `Round()` ממחלקת Math
```PowerShell
[Math]::Round(15.68) # מעגל ל-16
```
- ציון עשרוניים:
```PowerShell
[Math]::Round(15.684, 2) # מעגל ל-15.68
```
- `Ceiling()` ו-`Floor()`, לעיגול תמיד למעלה או למטה:
```PowerShell
[Math]::Ceiling(15.2) # מעגל למעלה ל-16
[Math]::Floor(15.9) # מעגל למטה ל-15
```

## צלילה עמוקה
עיגול מספרים אינו חדש; הוא קיים מזה זמנים עתיקים, שימושי למסחר, מדע ומדידת זמן. כאשר מדובר ב-PowerShell, `[Math]::Round()` משתמשת ב"עיגול הבנקאי" כברירת מחדל, שבו 0.5 עובר למספר הזוגי הקרוב ביותר, מה שמפחית הטיה בפעולות סטטיסטיות.

אתה לא תקוע רק עם שיטות `[Math]`. רוצה שליטה רבה יותר? בדוק את `[System.Math]::Round(Number, Digits, MidpointRounding)` שם אתה יכול לקבוע איך מרכזיות מטופלות: הרחק מאפס או לכיוון הזוגי (כלומר עיגול הבנקאי).

זווית נוספת: האובייקט `System.Globalization.CultureInfo`. הוא עוזר עם עיצוב והעדפות עיגול ספציפיות לאזור כאשר מתמודדים עם מספרים בינלאומיים.

## ראה גם
- המסמכים הרשמיים של מיקרוסופט על שיטות Math: [קישור](https://learn.microsoft.com/en-us/dotnet/api/system.math?view=net-7.0)
- פרטיות עיגול עשרוני ב-.NET: [קישור](https://learn.microsoft.com/en-us/dotnet/api/system.midpointrounding?view=net-7.0)
- דיונים על עיגול ב-StackOverflow: [קישור](https://stackoverflow.com/questions/tagged/rounding+powershell)

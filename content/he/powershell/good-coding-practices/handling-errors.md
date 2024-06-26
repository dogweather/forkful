---
date: 2024-01-26 01:00:06.793355-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: -PowerShell \u05D4\
  \u05EA\u05E4\u05EA\u05D7 \u05D4\u05E8\u05D1\u05D4 \u05DE\u05D0\u05D6 \u05D4\u05D9\
  \u05D5\u05D5\u05E1\u05D3\u05D5 \u05DB-Monad. \u05E0\u05D9\u05D4\u05D5\u05DC \u05E9\
  \u05D2\u05D9\u05D0\u05D5\u05EA \u05D4\u05D5\u05E4\u05DA \u05DC\u05DE\u05EA\u05D5\
  \u05D7\u05DB\u05DD \u05D9\u05D5\u05EA\u05E8 \u05E2\u05DD \u05D4\u05D6\u05DE\u05DF\
  , \u05D5\u05D4\u05D5\u05D0 \u05DE\u05E6\u05D9\u05E2 \u05EA\u05DB\u05D5\u05E0\u05D5\
  \u05EA \u05D3\u05D5\u05DE\u05D5\u05EA \u05DC\u05E9\u05E4\u05D5\u05EA \u05EA\u05DB\
  \u05E0\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA. \u05EA\u05D7\u05D1\u05D9\u05E8\
  \ `try-catch-\u2026"
lastmod: '2024-04-05T21:53:40.806594-06:00'
model: gpt-4-1106-preview
summary: "-PowerShell \u05D4\u05EA\u05E4\u05EA\u05D7 \u05D4\u05E8\u05D1\u05D4 \u05DE\
  \u05D0\u05D6 \u05D4\u05D9\u05D5\u05D5\u05E1\u05D3\u05D5 \u05DB-Monad."
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
weight: 16
---

## איך לעשות:
```PowerShell
# ניסיון-תפיסה בסיסי לטיפול בחריגות
try {
    # קוד שעלול להפעיל שגיאה
    $result = 1 / 0
} catch {
    # מה לעשות אם אירעה שגיאה
    Write-Host "אופס, אירעה שגיאה: $_"
}

# הצגת הודעת שגיאה מותאמת אישית
try {
    Get-Item "nonexistentfile.txt" -ErrorAction Stop
} catch {
    Write-Host "הקובץ לא נמצא."
}

# שימוש במשתנה $Error לבדיקת השגיאה האחרונה
```

## צלילה עמוקה
-PowerShell התפתח הרבה מאז היווסדו כ-Monad. ניהול שגיאות הופך למתוחכם יותר עם הזמן, והוא מציע תכונות דומות לשפות תכנות אחרות. תחביר `try-catch-finally` הוא אחת התכונות המשותפות משפות כמו C#. לפניו, תסריטאים הסתמכו במידה רבה על בדיקת תנאים ושימוש במשתנה האוטומטי $Error.

-PowerShell גם יש שני סוגים עיקריים של שגיאות: מסיימות ולא מסיימות. שגיאות מסיימות יעצרו את הסקריפט אלא אם כן הן נתפסו בבלוק `try-catch`, בעוד שגיאות לא מסיימות לא יעצרו אלא אם כן ציינת `-ErrorAction Stop`. ההבחנה הזו חשובה משום שהיא מעניקה שליטה דקדקנית על ניהול השגיאות, ומחליטה האם שגיאה באמת מצדיקה השבתת הסקריפט כולו או שניתן פשוט לרשום ולהתעלם.

ניהול שגיאות ב-PowerShell גם מאפשר שימוש בבלוק `finally`, שירוץ ללא קשר למה שקורה - בין אם אירעה שגיאה או לא. זה מעולה למשימות ניקוי.

כשאתם עמוקים בחפירות של הכתיבת תסריטים, אתם יכולים גם לטפל בסוגים ספציפיים של חריגות, ולקבל בזה שליטה עוד יותר דקדקנית.

לחלופין, יש את אפשרות העתיקה `-ErrorVariable` לזיהוי שגיאות ללא הפעלת חריגה. והמשתנה `$?` אומר לכם אם הפעולה האחרונה הייתה מוצלחת. הם כלים נוחים, אך קצת פחות נקיים ממבנה `try-catch` רציני.

## ראו גם
- [about_Try_Catch_Finally](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)

---
date: 2024-01-26 01:08:24.412367-07:00
description: "\u05DC\u05D5\u05D2\u05D9\u05E0\u05D2 \u05D6\u05D5 \u05D1\u05E2\u05E6\
  \u05DD \u05D4\u05E9\u05D0\u05E8\u05EA \u05E2\u05E7\u05D1\u05D5\u05EA \u05DC\u05D7\
  \u05DD \u05D1\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA - \u05D6\u05D5 \u05D4\u05D3\u05E8\
  \u05DA \u05E9\u05DC\u05DA \u05DC\u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8\u05D9\
  \ \u05DE\u05D4 \u05E9\u05E7\u05D5\u05E8\u05D4 \u05DB\u05E9\u05D4\u05E1\u05E7\u05E8\
  \u05D9\u05E4\u05D8 \u05E9\u05DC\u05DA \u05DE\u05EA\u05D1\u05E6\u05E2 \u05D1\u05E9\
  \u05D8\u05D7. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\
  \u05DD \u05E8\u05D9\u05E9\u05D5\u05DD \u05D1\u05DC\u05D5\u05D2\u05D9\u05DD \u05DB\
  \u05D3\u05D9 \u05DC\u05D0\u05D1\u05D7\u05DF \u05D1\u05D0\u05D2\u05D9\u05DD, \u05DC\
  \u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8\u05D9\u2026"
lastmod: '2024-03-11T00:14:13.196007-06:00'
model: gpt-4-1106-preview
summary: "\u05DC\u05D5\u05D2\u05D9\u05E0\u05D2 \u05D6\u05D5 \u05D1\u05E2\u05E6\u05DD\
  \ \u05D4\u05E9\u05D0\u05E8\u05EA \u05E2\u05E7\u05D1\u05D5\u05EA \u05DC\u05D7\u05DD\
  \ \u05D1\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA - \u05D6\u05D5 \u05D4\u05D3\u05E8\u05DA\
  \ \u05E9\u05DC\u05DA \u05DC\u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8\u05D9 \u05DE\
  \u05D4 \u05E9\u05E7\u05D5\u05E8\u05D4 \u05DB\u05E9\u05D4\u05E1\u05E7\u05E8\u05D9\
  \u05E4\u05D8 \u05E9\u05DC\u05DA \u05DE\u05EA\u05D1\u05E6\u05E2 \u05D1\u05E9\u05D8\
  \u05D7. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD\
  \ \u05E8\u05D9\u05E9\u05D5\u05DD \u05D1\u05DC\u05D5\u05D2\u05D9\u05DD \u05DB\u05D3\
  \u05D9 \u05DC\u05D0\u05D1\u05D7\u05DF \u05D1\u05D0\u05D2\u05D9\u05DD, \u05DC\u05E2\
  \u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8\u05D9\u2026"
title: "\u05E8\u05D9\u05E9\u05D5\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA (\u05DC\
  \u05D5\u05D2\u05D9\u05DD)"
---

{{< edit_this_page >}}

## מה ולמה? 
לוגינג זו בעצם השארת עקבות לחם בקוד שלך - זו הדרך שלך לעקוב אחרי מה שקורה כשהסקריפט שלך מתבצע בשטח. תכנתים מבצעים רישום בלוגים כדי לאבחן באגים, לעקוב אחרי התנהלות האפליקציה, לנטר ביצועים ולשים לב לכל משוכה.

## איך לעשות:
הנה המדריך לפיזור רישום בסיסי בתוך הסקריפטים שלך:

```PowerShell
# יצירת הודעת לוג פשוטה
Write-Host "Info: Starting the script process."

# כתיבה לקובץ
"Info: This is a logged message." | Out-File -Append myLog.log

# שימוש בפקודה המובנית לרישום מפורט יותר
Start-Transcript -Path "./detailedLog.log"
Write-Output "Warning: Something isn't quite right."
# ... הסקריפט שלך עושה דברים
Stop-Transcript

# פלט של detailedLog.log
******************************
Windows PowerShell transcript start
Start time: 20230324112347
Username  : PShellGuru@example.com
RunAs User: PShellGuru@example.com
Configuration Name: 
Machine  : PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
Host Application: C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
Process ID: 2024
PS Version: 7.1.2
```

עכשיו, בלוגים שלך יש תיעוד שלב אחר שלב של מה שהקוד שלך עשה.

## צלילה עמוקה:
היסטורית, לוגינג זה כמו תוכנות עצמן. זה כמו יומן ראש המשמרת אבל לתוכנה. בימים הראשונים, זה כנראה היה הדפסות או מכונות טלטייפ; כיום זה כל הקטע של קבצים ומערכות ניהול לוגים יוקרתיות.

כשאתה בחפירות של PowerShell, `Write-Host` זה מהיר ולא נקיון, אבל זה רק מוציא טקסט לקונסול, לא נהדר לשמירת רישומים. `Out-File` נותן לך דרך פשוטה להשליך טקסט לקובץ, אבל לשטף האמיתי, תרצה `Start-Transcript` ו-`Stop-Transcript` שמרשמים הכל - קלט, פלט, הכול.

אלטרנטיבות? בטח, אם אתה עובד בסביבה ארגונית, ייתכן שתעיף מבט ביומן אירועים של Windows או שתשתמש בתוכנה כמו Logstash, אבל לסקריפט היומיומי שלך, תישאר עם הכלים של PowerShell. ולגבי הטמעה, זכור לרשום בחכמה - מעט מדי וזה לא שימושי, יותר מדי וזה רעש לבן.

## ראה גם:
בדוק את אלה כדי להבין את כל הנושאים של רישום ב-PowerShell:

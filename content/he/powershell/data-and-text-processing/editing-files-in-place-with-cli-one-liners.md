---
date: 2024-01-27 16:21:14.937863-07:00
description: "\u05E2\u05E8\u05D9\u05DB\u05EA \u05E7\u05D1\u05E6\u05D9\u05DD \u05D1\
  \u05DE\u05E7\u05D5\u05DD \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E9\u05D5\
  \u05E8\u05D5\u05EA \u05E4\u05E7\u05D5\u05D3\u05D4 \u05D7\u05D3 \u05E4\u05E2\u05DE\
  \u05D9\u05D5\u05EA \u05D1-PowerShell \u05D4\u05D9\u05D0 \u05E2\u05DC \u05D9\u05D3\
  \u05D9 \u05D1\u05D9\u05E6\u05D5\u05E2 \u05E9\u05D9\u05E0\u05D5\u05D9\u05D9\u05DD\
  \ \u05D9\u05E9\u05D9\u05E8\u05D9\u05DD \u05D1\u05E7\u05D1\u05E6\u05D9\u05DD \u05DE\
  \u05D4\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4, \u05DC\u05DC\
  \u05D0 \u05D4\u05E6\u05D5\u05E8\u05DA \u05DC\u05E4\u05EA\u05D5\u05D7 \u05D0\u05D5\
  \u05EA\u05DD \u05D1\u05E2\u05D5\u05E8\u05DA. \u05D2\u05D9\u05E9\u05D4 \u05D6\u05D5\
  \u2026"
lastmod: '2024-03-13T22:44:39.691799-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05E8\u05D9\u05DB\u05EA \u05E7\u05D1\u05E6\u05D9\u05DD \u05D1\u05DE\
  \u05E7\u05D5\u05DD \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E9\u05D5\u05E8\
  \u05D5\u05EA \u05E4\u05E7\u05D5\u05D3\u05D4 \u05D7\u05D3 \u05E4\u05E2\u05DE\u05D9\
  \u05D5\u05EA \u05D1-PowerShell \u05D4\u05D9\u05D0 \u05E2\u05DC \u05D9\u05D3\u05D9\
  \ \u05D1\u05D9\u05E6\u05D5\u05E2 \u05E9\u05D9\u05E0\u05D5\u05D9\u05D9\u05DD \u05D9\
  \u05E9\u05D9\u05E8\u05D9\u05DD \u05D1\u05E7\u05D1\u05E6\u05D9\u05DD \u05DE\u05D4\
  \u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4, \u05DC\u05DC\u05D0\
  \ \u05D4\u05E6\u05D5\u05E8\u05DA \u05DC\u05E4\u05EA\u05D5\u05D7 \u05D0\u05D5\u05EA\
  \u05DD \u05D1\u05E2\u05D5\u05E8\u05DA."
title: "\u05E2\u05E8\u05D9\u05DB\u05EA \u05E7\u05D1\u05E6\u05D9\u05DD \u05D1\u05DE\
  \u05E7\u05D5\u05DD \u05E2\u05DD \u05E9\u05D5\u05E8\u05D5\u05EA \u05E4\u05E7\u05D5\
  \u05D3\u05D4 \u05D7\u05D3-\u05E9\u05D5\u05E8\u05EA\u05D9\u05D5\u05EA"
weight: 32
---

## איך ל:


### החלפת טקסט בקובץ יחיד
בואו נתחיל במשימה פשוטה: אתם רוצים להחליף את כל מופעי "oldtext" ב"newtext" בקובץ בשם example.txt. הנה איך עושים את זה:

```PowerShell
(Get-Content example.txt) -replace 'oldtext', 'newtext' | Set-Content example.txt
```

שורת הפקודה הזו קוראת את התוכן, מבצעת את ההחלפה וכותבת את התוכן חזרה לקובץ המקורי.

### עריכת מספר קבצים
מה אם אתם צריכים להחיל את אותה השינוי על מספר קבצים? הנה גישה באמצעות לולאה:

```PowerShell
Get-ChildItem *.txt | ForEach-Object {
  (Get-Content $_) -replace 'oldtext', 'newtext' | Set-Content $_
}
```

קטע קוד זה מחפש את כל קבצי ה`.txt` בתיקייה הנוכחית, מחליף את "oldtext" ב"newtext" בכל אחד מהם.

### הוספת תוכן בתחילה או בסוף קבצים
הוספה או הקדמה של תוכן יכולה גם להתבצע ביעילות:

```PowerShell
# הקדמה
"New first line`n" + (Get-Content example.txt) | Set-Content example.txt

# הוספה
(Get-Content example.txt) + "`nNew last line" | Set-Content example.txt
```

כאן, אנו פשוט מצרפים את התוכן החדש לפני או אחרי התוכן הקיים ושומרים אותו חזרה.

## עיון מעמיק
באופן היסטורי, עריכה במקום נקשרת יותר עם כלים של Unix כמו `sed` ו-`awk`. PowerShell, שהוא כלי יותר חדש, לא כולל תכונת עריכה במקום מובנית מראש. זה נובע במידה רבה מפילוסופיית העיצוב שלו, המדגישה את חשיבות האובייקטים על פני זרמי טקסט, בניגוד לכלים של Unix שמתייחסים לרוב הקלטים כטקסט.

חלופות ל-PowerShell למשימה זו כוללות שימוש בכלים קלאסיים של Unix הזמינים ב-Windows דרך Cygwin או מערכת ההפעלה המשנית של Windows ל-Linux (WSL). כלים אלו לעיתים קרובות מספקים תחביר יותר תמציתי לעיבוד במקום בזכות העיצוב הממוקד טקסט שלהם.

מבחינה טכנית, חשוב לציין כי גישת PowerShell כוללת קריאה של הקובץ כולו לזיכרון, ביצוע השינויים, ואז כתיבה חזרה. למרות שזה עובד היטב לקבצים בגודל בינוני, זה יכול להיות לא יעיל לקבצים גדולים מאוד. במקרים כאלה, כדאי לשקול שימוש בשיטות של `.NET` ישירות או פנייה לכלים אלטרנטיביים המיועדים לעיבוד זרמים גדולים של נתונים.

למרות אלו שיקולים, גמישותה והמאפיינים הנרחבים של PowerShell הופכים אותו לכלי יקר ערך למניפולציה של קבצים ישירות משורת הפקודה, במיוחד עבור מי שכבר מוטבע באקוסיסטם של Windows או מנהל סביבות רב-פלטפורמיות.

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:34.230605-07:00
description: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D9\u05E9\u05D9\u05D5\u05EA\
  \ \u05D1-PowerShell \u05DB\u05D5\u05DC\u05DC\u05EA \u05D4\u05DE\u05E8\u05D4 \u05E9\
  \u05DC \u05D4\u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\u05DF \u05E9\u05DC \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05E0\u05EA\u05D5\u05E0\u05D4 \u05DC\u05D0\u05D5\
  \u05EA \u05D2\u05D3\u05D5\u05DC\u05D4, \u05EA\u05D5\u05DA \u05D4\u05E9\u05D0\u05E8\
  \u05EA \u05E9\u05D0\u05E8 \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05DC\
  \u05D0 \u05E9\u05D9\u05E0\u05D5\u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.666801-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D9\u05E9\u05D9\u05D5\u05EA \u05D1\
  -PowerShell \u05DB\u05D5\u05DC\u05DC\u05EA \u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC\
  \ \u05D4\u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\u05DF \u05E9\u05DC \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05E0\u05EA\u05D5\u05E0\u05D4 \u05DC\u05D0\u05D5\u05EA\
  \ \u05D2\u05D3\u05D5\u05DC\u05D4, \u05EA\u05D5\u05DA \u05D4\u05E9\u05D0\u05E8\u05EA\
  \ \u05E9\u05D0\u05E8 \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05DC\u05D0\
  \ \u05E9\u05D9\u05E0\u05D5\u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\
  \u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA\u2026"
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
הפיכת מחרוזת לאותיות רישיות ב-PowerShell כוללת המרה של התו הראשון של מחרוזת נתונה לאות גדולה, תוך השארת שאר המחרוזת ללא שינוי. מתכנתים לעיתים קרובות מבצעים משימה זו לצורכי עיצוב, כגון הכנת טקסט להצגה בממשקי משתמש או עמידה בכללים דקדוקיים במסמכים שנוצרו.

## איך לעשות זאת:
PowerShell, הוא כלי גמיש, מאפשר לך להפוך מחרוזת לאותיות ראשיות באמצעות שיטות ישירות ללא הצורך בספריות צד שלישי. כך אתה יכול לעשות זאת:

```powershell
# באמצעות שיטת ה-.Net המובנית 'ToTitleCase' מ-CultureInfo
$text = "hello world"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
פלט:
```
Hello world
```

שימו לב: שיטה זו מפיקה את האות הראשונה של כל מילה לאות ראשית. אם אתה רוצה במפורש לפיקתי רק את האות הראשונה של המחרוזת ולהשאיר את שאר המחרוזת כפי שהיא, תוכל לעשות משהו כזה:

```powershell
# פיכתי רק את התו הראשון של מחרוזת לאות רישית
$text = "hello world"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
פלט:
```
Hello world
```

PowerShell אינו כולל באופן ישיר פונקציה פשוטה לפיכתי רק את האות הראשונה של מחרוזת לאות רישית, אבל על ידי שילוב שיטות יסודיות של עיבוד מחרוזות כמו `Substring(0,1).ToUpper()` ושרשור, אנחנו יכולים להשיג בקלות את התוצאה הרצויה.

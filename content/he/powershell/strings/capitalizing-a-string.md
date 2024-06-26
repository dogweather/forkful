---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:34.230605-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : PowerShell, \u05D4\u05D5\u05D0 \u05DB\u05DC\u05D9 \u05D2\u05DE\u05D9\u05E9, \u05DE\
  \u05D0\u05E4\u05E9\u05E8 \u05DC\u05DA \u05DC\u05D4\u05E4\u05D5\u05DA \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D0\
  \u05E9\u05D9\u05D5\u05EA \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E9\u05D9\
  \u05D8\u05D5\u05EA \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05DC\u05DC\u05D0 \u05D4\
  \u05E6\u05D5\u05E8\u05DA \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\u05D3\
  \ \u05E9\u05DC\u05D9\u05E9\u05D9. \u05DB\u05DA \u05D0\u05EA\u05D4 \u05D9\u05DB\u05D5\
  \u05DC \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA."
lastmod: '2024-03-13T22:44:39.666801-06:00'
model: gpt-4-0125-preview
summary: "PowerShell, \u05D4\u05D5\u05D0 \u05DB\u05DC\u05D9 \u05D2\u05DE\u05D9\u05E9\
  , \u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DA \u05DC\u05D4\u05E4\u05D5\u05DA \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\
  \u05D0\u05E9\u05D9\u05D5\u05EA \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E9\
  \u05D9\u05D8\u05D5\u05EA \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05DC\u05DC\u05D0\
  \ \u05D4\u05E6\u05D5\u05E8\u05DA \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\
  \u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9."
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 2
---

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

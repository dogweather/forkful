---
title:                "הגדלת אותיות במחרוזת"
date:                  2024-02-03T19:06:34.230605-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:59.796252-07:00
description: "\u05E4\u05D9\u05E8\u05D5\u05E9 HTML \u05D1-PowerShell \u05E2\u05D5\u05E1\
  \u05E7 \u05D1\u05E4\u05D9\u05E8\u05D5\u05E7 \u05EA\u05D5\u05DB\u05DF HTML \u05DB\
  \u05D3\u05D9 \u05DC\u05D7\u05DC\u05E5 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E1\
  \u05E4\u05E6\u05D9\u05E4\u05D9\u05D9\u05DD \u05D0\u05D5 \u05DC\u05D0\u05D5\u05D8\
  \u05D5\u05DE\u05D8 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E7\u05E9\u05D5\u05E8\
  \u05D5\u05EA \u05DC\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05EA\u05D5\u05DB\
  \u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05E2\u05E8\u05D1 \u05D1\u05D3\u05E4\
  \u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8,\u2026"
lastmod: '2024-03-13T22:44:39.694983-06:00'
model: gpt-4-0125-preview
summary: "\u05E4\u05D9\u05E8\u05D5\u05E9 HTML \u05D1-PowerShell \u05E2\u05D5\u05E1\
  \u05E7 \u05D1\u05E4\u05D9\u05E8\u05D5\u05E7 \u05EA\u05D5\u05DB\u05DF HTML \u05DB\
  \u05D3\u05D9 \u05DC\u05D7\u05DC\u05E5 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E1\
  \u05E4\u05E6\u05D9\u05E4\u05D9\u05D9\u05DD \u05D0\u05D5 \u05DC\u05D0\u05D5\u05D8\
  \u05D5\u05DE\u05D8 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E7\u05E9\u05D5\u05E8\
  \u05D5\u05EA \u05DC\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05EA\u05D5\u05DB\
  \u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05E2\u05E8\u05D1 \u05D1\u05D3\u05E4\
  \u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8,\u2026"
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
---

{{< edit_this_page >}}

## מה ולמה?
פירוש HTML ב-PowerShell עוסק בפירוק תוכן HTML כדי לחלץ נתונים ספציפיים או לאוטומט פעולות קשורות לאינטרנט. תוכניתנים עושים זאת כדי להתערב בדפי אינטרנט, לגרד תוכן מהרשת, או לאוטומט הגשות טפסים ואינטראקציות אחרות ברשת ללא צורך בדפדפן.

## איך לעשות:

ב-PowerShell אין מנתח HTML מוקדש כחלק מהמערכת, אך ניתן להשתמש בפקודת ה-`Invoke-WebRequest` כדי לגשת ולפרש תוכן HTML. לצורך פירוש ותיקון מורכב יותר, ניתן להשתמש ב-HtmlAgilityPack, ספריית .NET פופולרית.

### שימוש ב-`Invoke-WebRequest`:

```powershell
# דוגמה פשוטה לאיסוף כותרות מדף אינטרנט
$response = Invoke-WebRequest -Uri 'http://example.com'
# השתמש במאפיין ParsedHtml לגישה לאלמנטים של DOM
$title = $response.ParsedHtml.title
Write-Output $title
```

פלט דוגמה:

```
Example Domain
```

### שימוש ב-HtmlAgilityPack:

ראשית, צריך להתקין את HtmlAgilityPack. ניתן לעשות זאת דרך מנהל החבילות NuGet:

```powershell
Install-Package HtmlAgilityPack -ProviderName NuGet
```

לאחר מכן, ניתן להשתמש בו ב-PowerShell לפרש HTML:

```powershell
# טען את ספריית HtmlAgilityPack
Add-Type -Path "path\to\HtmlAgilityPack.dll"

# צור אובייקט מסוג HtmlDocument
$doc = New-Object HtmlAgilityPack.HtmlDocument

# טען HTML מקובץ או בקשת אינטרנט
$htmlContent = (Invoke-WebRequest -Uri "http://example.com").Content
$doc.LoadHtml($htmlContent)

# השתמש ב-XPath או שיטות שאילתא אחרות לחילוץ אלמנטים
$node = $doc.DocumentNode.SelectSingleNode("//h1")

if ($node -ne $null) {
    Write-Output $node.InnerText
}
```

פלט דוגמה:

```
!ברוכים הבאים ל-Example.com
```

בדוגמאות אלו, `Invoke-WebRequest` מתאים ביותר למשימות פשוטות, בעוד ש-HtmlAgilityPack מציעה מערכת עשירה יותר של כלים לפירוש ותיקון מורכב של HTML.

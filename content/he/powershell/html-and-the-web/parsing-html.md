---
title:                "פיענוח HTML"
aliases:
- /he/powershell/parsing-html.md
date:                  2024-02-03T19:12:59.796252-07:00
model:                 gpt-4-0125-preview
simple_title:         "פיענוח HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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

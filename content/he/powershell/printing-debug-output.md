---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
הדפסת פלט ניפוי, מכונה גם 'Debug Output', היא שיטה שבה מתכנתים משלבים הודעות בקוד כדי לבחון ולעקוב אחריהם. העקרונות העומדים מאחוריהם הם לסייע בניפוי (Debugging) ולאבחון בעיות בזמן ריצה.

## איך לעשות זאת:
הנה דוגמה מהירה של כיצד להשתמש ב-Write-Debug ב-PowerShell.

```PowerShell
function Test-Debug {
    [CmdletBinding()]
    param()

    Write-Debug "Start of function"
    $exampleVariable = Get-Date
    Write-Debug "Created variable with value: $exampleVariable"
}
$DebugPreference = 'Continue'
Test-Debug
```

הפלט של הקוד הזה:

```PowerShell
DEBUG: Start of function
DEBUG: Created variable with value: 10/02/2022 10:50:12
```

## צלילה עמוקה
האילמנט של הדפסת פלט לניפוי נמשך במשך שנים רבות, והוא נפוץ במרבית השפות. ב-PowerShell, אנחנו משתמשים ב-Write-Debug, אך ישנן אפשרויות חלופיות כמו Write-Verbose או Write-Information.

הפונקציה Write-Debug משתמשת במשתנה $DebugPreference לקביעת ההתנהגות. הואייחודי למחצית האחרונה של PowerShell (מהגרסא 5 והלאה), ואפשרויותיו הן 'SilentlyContinue', 'Stop', 'Continue', ו-'Inquire'.

## ראה גם
[הסברים נוספים על Debugging ב-PowerShell](https://docs.microsoft.com/he-il/powershell/scripting/learn/debugging-from-command-line)
[מידע נוסף על Write-Debug](https://docs.microsoft.com/he-il/powershell/module/microsoft.powershell.utility/write-debug)
---
title:                "הפיכת מחרוזת לאותיות רישיות"
date:                  2024-01-19
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
קידוד אותיות בגדול הוא ההפיכה של מחרוזת לאותיות ראשיות גדולות. תכניתנים עושים את זה לנורמליזציה של טקסט, לדוגמה, כשמבצעים השוואות או כשכותבים בתבניות עקביות למשתמש.

## How to (איך לעשות:)
```PowerShell
# Capitalizing a single word
$word = "shalom"
$capitalizedWord = $word.Substring(0,1).ToUpper() + $word.Substring(1).ToLower()
$capitalizedWord # Outputs: Shalom

# Capitalizing each word in a string
$sentence = "baruch haba l'yerushalayim"
$capitalizedSentence = $sentence -split ' ' | % { $_.Substring(0,1).ToUpper() + $_.Substring(1).ToLower() } -join ' '
$capitalizedSentence # Outputs: Baruch Haba L'Yerushalayim
```

## Deep Dive (צלילה עמוקה)
בעבר, פונקציונליות להפיכת טקסט לאותיות ראשיות גדולות הייתה חלק ממערכות עיבוד טקסט ומערכות דואר אלקטרוני בשביל טפלוגרפיה נכונה. ב-PowerShell, אנחנו עושים זאת עם שילוב של מתודות המחלקה String של .NET. אלטרנטיבות כוללות שימוש ב cmdlet `ToUpper()` להפוך את המחרוזת שלמה לאותיות גדולות או cmdlet `ToLower()` לאותיות קטנות, ואז עיבוד נוסף להפיכה של האותיות הראשונות בכל מילה לגדולה. פעולה זו אינה כלולה ב-PowerShell באופן ישיר אך תמיד ניתן לבנות פונקציה מותאמת אישית.

## See Also (ראה גם)
- [About Automatic Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables) - מידע על משתנים אוטומטיים ב-PowerShell.
- [.NET String Class](https://docs.microsoft.com/en-us/dotnet/api/system.string) - מידע מעמיק על המחלקה String ב-.NET.

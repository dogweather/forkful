---
title:                "המרת מחרוזת לאותיות קטנות"
date:                  2024-01-20T17:39:11.300843-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"

category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? - מה ולמה?
להמיר מחרוזת לאותיות קטנות זה פשוט לשנות את כל האותיות הגדולות בתוך מחרוזת לגרסתן הקטנה. תוכניתנים עושים את זה לשם אחידות, למנוע בעיות בהשוואות רגישות לרישיות, ולעיבוד נתונים.

## How to - איך לעשות:
קוד PowerShell פשוט לשינוי לאותיות קטנות:

```PowerShell
$string = "Shalom, Olam!"
$lowerCaseString = $string.ToLower()
$lowerCaseString
```

פלט לדוגמא:

```
shalom, olam!
```

## Deep Dive - צלילה לעומק:
להמיר מחרוזת לאותיות קטנות היא פעולה שנעשית כבר שנים רבות בתחום התכנות, המטרה שלה למנוע בעיות רגישות רישיות. ב-PowerShell, השימוש ב`.ToLower()` מבוסס על מתודה מ-.NET Framework. ישנן גישות חלופיות, כמו הוספת `[cultureinfo]::InvariantCulture.TextInfo` לעבוד עם תרבויות שונות או שימוש ב`.ToLowerInvariant()` לאחידות תרבותית. Implementing a custom lowercasing function is an overkill when the built-in methods are generally very efficient and account for edge cases and locale-specific rules.

## See Also - ראה גם:
- [.NET Globalization and Localization](https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/)
- [PowerShell Scripting](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.2)
- [String manipulation best practices in PowerShell](https://devblogs.microsoft.com/scripting/)`

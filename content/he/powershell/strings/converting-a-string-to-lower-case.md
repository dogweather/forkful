---
date: 2024-01-20 17:39:11.300843-07:00
description: "\u05DC\u05D4\u05DE\u05D9\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D6\
  \u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\u05EA \u05DB\
  \u05DC \u05D4\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D4\u05D2\u05D3\u05D5\u05DC\
  \u05D5\u05EA \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D2\u05E8\u05E1\u05EA\u05DF \u05D4\u05E7\u05D8\u05E0\u05D4. \u05EA\u05D5\u05DB\
  \u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA\
  \ \u05D6\u05D4 \u05DC\u05E9\u05DD \u05D0\u05D7\u05D9\u05D3\u05D5\u05EA, \u05DC\u05DE\
  \u05E0\u05D5\u05E2 \u05D1\u05E2\u05D9\u05D5\u05EA \u05D1\u05D4\u05E9\u05D5\u05D5\
  \u05D0\u05D5\u05EA \u05E8\u05D2\u05D9\u05E9\u05D5\u05EA\u2026"
lastmod: '2024-03-11T00:14:13.159225-06:00'
model: gpt-4-1106-preview
summary: "\u05DC\u05D4\u05DE\u05D9\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D6\u05D4\
  \ \u05E4\u05E9\u05D5\u05D8 \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\u05EA \u05DB\u05DC\
  \ \u05D4\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D4\u05D2\u05D3\u05D5\u05DC\u05D5\
  \u05EA \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D2\
  \u05E8\u05E1\u05EA\u05DF \u05D4\u05E7\u05D8\u05E0\u05D4. \u05EA\u05D5\u05DB\u05E0\
  \u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\
  \u05D4 \u05DC\u05E9\u05DD \u05D0\u05D7\u05D9\u05D3\u05D5\u05EA, \u05DC\u05DE\u05E0\
  \u05D5\u05E2 \u05D1\u05E2\u05D9\u05D5\u05EA \u05D1\u05D4\u05E9\u05D5\u05D5\u05D0\
  \u05D5\u05EA \u05E8\u05D2\u05D9\u05E9\u05D5\u05EA\u2026"
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
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

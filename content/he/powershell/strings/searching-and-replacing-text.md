---
date: 2024-01-20 17:58:52.164498-07:00
description: "How to: (\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA:) \u05E4\u05DC\
  \u05D8."
lastmod: '2024-04-05T21:53:40.777577-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
weight: 10
---

## How to: (איך לעשות:)
```PowerShell
# חיפוש והחלפה במחרוזת
$text = "שלום, עולם!"
$newText = $text -replace 'עולם', 'כולם'
$newText
```
פלט:
```
שלום, כולם!
```

```PowerShell
# חיפוש והחלפה בקובץ טקסט
(Get-Content ./example.txt) -replace 'ישן', 'חדש' | Set-Content ./example.txt
```

## Deep Dive (עומק השקעה)
הכלי `-replace` ב-PowerShell מבוסס על ביטויים רגולריים (regular expressions), שנוספו לשפות תכנות בשנות ה-60 ומאפשרים חיפוש פלטרני ומורכב. יש גם אלטרנטיבות כמו `String.Replace()`, אבל `-replace` חזק יותר. חשוב לשים לב: `-replace` יוצר מחרוזת חדשה, הוא לא משנה את המחרוזת המקורית.

## See Also (ראה גם)
- [דוקומנטציה של מחלקת String ב-.NET](https://docs.microsoft.com/dotnet/api/system.string)

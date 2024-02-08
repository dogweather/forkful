---
title:                "חיפוש והחלפת טקסט"
date:                  2024-01-20T17:58:52.164498-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
חיפוש והחלפה של טקסט זה פשוט: מוצאים מחרוזת טקסט ומחליפים אותה באחרת. תכניתנים עושים את זה כדי לעדכן נתונים, לתקן שגיאות, או לעשות שינויים גורפים בקבצים.

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

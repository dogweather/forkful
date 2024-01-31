---
title:                "מציאת אורך מחרוזת"
date:                  2024-01-20T17:47:53.986968-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מציאת אורך של מחרוזת היא פשוט לבדוק כמה תווים יש בה. תכניתנים עושים את זה כדי למנוע שגיאות, לולאות אינסופיות, ולהתמודד עם קלט משתמש.

## איך לעשות:

למצוא אורך של מחרוזת ב-PowerShell, השתמש במאפיין `.Length`. להלן דוגמא:

```PowerShell
$myString = 'שלום עולם'
$length = $myString.Length
$length
```

תוצאה:

```
9
```

## עומק המידע:

השימוש במאפיין `.Length` קיים כבר מהגרסאות הראשונות של PowerShell והוא בא מהעולם של .NET. ישנן אלטרנטיבות, כמו פונקציות מותאמות אישית שאתה כותב בעצמך, אבל רוב הזמן אין צורך בזה. כשמבצעים אופרציות על מחרוזות, חשוב לדעת את האורך שלהן כדי למנוע גישה לאינדקס שלא קיים ולגרום לבעיות.

## ראה גם:

- [אתר StackOverflow עם שאלות ותשובות על PowerShell](https://stackoverflow.com/questions/tagged/powershell)

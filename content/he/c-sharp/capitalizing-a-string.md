---
title:                "הפיכת מחרוזת לאותיות רישיות"
date:                  2024-01-19
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
מיידע הוא להגדיל כל תו במחרוזת לאות גדולה. תוכניתנים עושים זאת לצורך עקביות, קריאות ולעיתים לציית לסטנדרטים של עיצוב טקסט.

## How to: (איך לעשות:)
ב-C#, התהליך של הפיכת מחרוזת לאותיות גדולות הוא פשוט. הנה דוגמה:

```C#
string message = "שלום עולם";
string capitalizedMessage = message.ToUpper(); // המרת כל האותיות לאותיות גדולות
Console.WriteLine(capitalizedMessage);
```

פלט דוגמה:
```
שלום עולם
```

## Deep Dive (עיון מעמיק)
הפונקציה `ToUpper()` ב-C# משתמשת בהגדרות התרבותיות (Culture) של המערכת כדי לקבוע אילו המרות לבצע. למשל, באנגלית, 'i' תתומר ל-'I', אבל בתורכית, יש אות גדולה נפרדת ל-'i' ללא נקודה.

אלטרנטיבה פופולארית ל- `ToUpper()` היא `ToLower()`, שהופכת את האותיות לקטנות.

ביצוע ההמרה נעשה על-פי קודי Unicode של התווים. מאחורי הקלעים, המערכת מוצאת את הקוד המתאים לאות הגדולה ומחליפה אותו במקום התו המקורי במחרוזת.

## See Also (ראה גם)
- [String.ToUpper Method in C#](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [Microsoft Docs on CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)
- [Unicode Character Table](https://unicode-table.com/)

---
title:                "מחיקת תווים התואמים לתבנית"
aliases: - /he/powershell/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:27.666296-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
הסרת תווים התואמים לתבנית זה פעולה פשוטה: פילטרציה של תווים ממחרוזת בהתאם לתבנית מסוימת. תכנתים עושים את זה לנקות קלט, להסיר תווים לא רצויים, או לעבד נתונים לפני שהם משמשים בתהליכים נוספים.

## איך לעשות:
נניח שאתה רוצה להסיר כל הפסיקים והנקודות ממחרוזת. הנה דוגמה פשוטה ב-PowerShell:

```PowerShell
$stringToClean = "שלום, עולם. זו מחרוזת עם פסיקים ונקודות."
$cleanString = $stringToClean -replace '[,.]', ''
$cleanString
```

תוצאה:

```
שלום עולם זו מחרוזת עם פסיקים ונקודות
```

למחיקת תווים שמתחילים או מסתיימים באות מסויימת:

```PowerShell
$stringToClean = "xזה מתחיל ונגמר ב-x"
$cleanString = $stringToClean -replace '^x|x$'
$cleanString
```

תוצאה:

```
זה מתחיל ונגמר ב-
```

## צלילה עמוקה
הסרת תווים בהתאם לתבנית היא למעשה תת-משימה של ביטויים רגולריים, טכניקה שפותחה בשנות ה-60. ב-PowerShell, הפעולה `-replace` מאפשרת שימוש בביטויים רגולריים, וזה יעיל במיוחד למשימות כמו זיהוי ומחיקה או החלפה של תבניות.

ישנן גם אלטרנטיבות פשוטות יותר להסרת תווים ממחרוזות, כמו שימוש בפעולות חיתוך דרך מיניפולציית אינדקסים או `String.Trim()` וגרסאותיה להסרת תווים ספציפיים בתחילת ובסוף מחרוזת.

לעומת זאת, ביטויים רגולריים מאפשרים דיוק וגמישות רבה יותר במציאת תבניות מורכבות ומחיקתן.

## ראו גם
- [מחרוזות וביטויים רגולריים ב-PowerShell ב-SS64](https://ss64.com/ps/syntax-regex.html)
- [איך להשתמש ב-String.Replace ב-PowerShell](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0#System_String_Replace_System_String_System_String_)

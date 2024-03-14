---
date: 2024-01-20 17:43:27.666296-07:00
description: "\u05D4\u05E1\u05E8\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA \u05D6\u05D4\
  \ \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4: \u05E4\u05D9\u05DC\
  \u05D8\u05E8\u05E6\u05D9\u05D4 \u05E9\u05DC \u05EA\u05D5\u05D5\u05D9\u05DD \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\u05D4\u05EA\u05D0\u05DD \u05DC\u05EA\
  \u05D1\u05E0\u05D9\u05EA \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA. \u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DC\
  \u05E0\u05E7\u05D5\u05EA \u05E7\u05DC\u05D8, \u05DC\u05D4\u05E1\u05D9\u05E8 \u05EA\
  \u05D5\u05D5\u05D9\u05DD \u05DC\u05D0 \u05E8\u05E6\u05D5\u05D9\u05D9\u05DD, \u05D0\
  \u05D5 \u05DC\u05E2\u05D1\u05D3\u2026"
lastmod: '2024-03-13T22:44:39.668464-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E1\u05E8\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\u05D5\
  \u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA \u05D6\u05D4 \u05E4\
  \u05E2\u05D5\u05DC\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4: \u05E4\u05D9\u05DC\u05D8\
  \u05E8\u05E6\u05D9\u05D4 \u05E9\u05DC \u05EA\u05D5\u05D5\u05D9\u05DD \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\u05D4\u05EA\u05D0\u05DD \u05DC\u05EA\u05D1\
  \u05E0\u05D9\u05EA \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA. \u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DC\u05E0\
  \u05E7\u05D5\u05EA \u05E7\u05DC\u05D8, \u05DC\u05D4\u05E1\u05D9\u05E8 \u05EA\u05D5\
  \u05D5\u05D9\u05DD \u05DC\u05D0 \u05E8\u05E6\u05D5\u05D9\u05D9\u05DD, \u05D0\u05D5\
  \ \u05DC\u05E2\u05D1\u05D3\u2026"
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
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

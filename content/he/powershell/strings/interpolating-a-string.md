---
aliases:
- /he/powershell/interpolating-a-string/
date: 2024-01-20 17:51:33.075548-07:00
description: "\u05DE\u05D9\u05DC\u05D5\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D4\u05D5\u05D0 \u05D8\u05DB\u05E0\u05D9\u05E7\u05D4 \u05DC\u05D4\u05DB\u05E0\
  \u05EA \u05D8\u05E7\u05E1\u05D8 \u05E2\u05DD \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD\
  \ \u05DE\u05D5\u05D8\u05DE\u05E2\u05D9\u05DD \u05D1\u05EA\u05D5\u05DB\u05D4. \u05EA\
  \u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05D4\u05D5\
  \u05D3\u05E2\u05D5\u05EA \u05D3\u05D9\u05E0\u05D0\u05DE\u05D9\u05D5\u05EA, \u05DC\
  \u05D5\u05D2\u05D9\u05DD, \u05D5\u05DC\u05D4\u05EA\u05D0\u05D9\u05DD \u05E4\u05DC\
  \u05D8 \u05D8\u05E7\u05E1\u05D8 \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D0\u05D5\
  \ \u05DC\u05EA\u05D4\u05DC\u05D9\u05DA."
lastmod: 2024-02-18 23:08:53.053967
model: gpt-4-1106-preview
summary: "\u05DE\u05D9\u05DC\u05D5\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D4\u05D5\u05D0 \u05D8\u05DB\u05E0\u05D9\u05E7\u05D4 \u05DC\u05D4\u05DB\u05E0\
  \u05EA \u05D8\u05E7\u05E1\u05D8 \u05E2\u05DD \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD\
  \ \u05DE\u05D5\u05D8\u05DE\u05E2\u05D9\u05DD \u05D1\u05EA\u05D5\u05DB\u05D4. \u05EA\
  \u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05D4\u05D5\
  \u05D3\u05E2\u05D5\u05EA \u05D3\u05D9\u05E0\u05D0\u05DE\u05D9\u05D5\u05EA, \u05DC\
  \u05D5\u05D2\u05D9\u05DD, \u05D5\u05DC\u05D4\u05EA\u05D0\u05D9\u05DD \u05E4\u05DC\
  \u05D8 \u05D8\u05E7\u05E1\u05D8 \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D0\u05D5\
  \ \u05DC\u05EA\u05D4\u05DC\u05D9\u05DA."
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
מילוי מחרוזות הוא טכניקה להכנת טקסט עם משתנים מוטמעים בתוכה. תוכניתנים עושים זאת כדי ליצור הודעות דינאמיות, לוגים, ולהתאים פלט טקסט למשתמש או לתהליך.

## איך לעשות:
ב-PowerShell, אנחנו משתמשים בסימני גרש (" ") למילוי מחרוזת ומשתמשים בתו `$` לפני שם משתנה כדי להטמיע אותו. דוגמה:

```PowerShell
$name = 'נעם'
$day = 'שני'
"greeting = שלום, $name! היום יום $day."
```

פלט:

```
greeting = שלום, נעם! היום יום שני.
```

למרות שזאת לא דרישה, כאשר משתמשים בביטויים מורכבים בתוך מחרוזות, נהוג להשתמש בסוגריים `${}`:

```PowerShell
$item = 'חלב'
$quantity = 3
"$quantity בקבוקי ${item} במקרר"
```

פלט:

```
3 בקבוקי חלב במקרר
```

## נפילה עמוקה
האפשרות למלא מחרוזות החלה כבר בשפות תכנות מוקדמות והגיעה לPowershell מייד עם הקדמה שלו. מיקרוסופט כללה זאת כדי להפוך את העבודה עם טקסט לפשוטה יותר. גרסאות קודמות של PowerShell השתמשו ב -fmt` למלוי` המחרוזת, אבל זה הפך לדי מיושן.

כאלטרנטיבה, ישנה האופציה להשתמש ב-fstring ב-Python, או String.Format ב-C#, אך ב-PowerShell, השימוש ב-$ היא הגישה המועדפת. כשמבצעים מילוי, המחרוזת מתרגמת לביטוי ואז מתבצעת, כאשר הפלט הוא הטקסט עם הערכים הרלוונטיים מהמשתנים.

## ראה גם
- [על מילוי מחרוזות ב-PowerShell](https://docs.microsoft.com/powershell/)
- [מילוי מחרוזות ומבני נתונים ב-PowerShell](https://ss64.com/ps/syntax-operators.html)

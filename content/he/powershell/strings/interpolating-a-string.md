---
title:                "שרבוב מחרוזת"
aliases: - /he/powershell/interpolating-a-string.md
date:                  2024-01-20T17:51:33.075548-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/interpolating-a-string.md"
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

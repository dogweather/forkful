---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

משלבים מחרוזת זה פעולה שבה אנו משנים חלק מהמחרוזת באמצעות נתונים ממשתנה מסוים. המתכנתים עושים את זה כדי לקבל מידע מתוך משתנה ולהוסיף אותו לתוך מחרודת באופן קל ומהיר.

## איך לעשות:

קוד דוגמא:

```PowerShell
$firstName = "דוד"
$lastName = "לוי"
Write-Output "שלום, $firstName $lastName"
```

הפלט יהיה:

```
שלום, דוד לוי
```

## צלילה עמוקה:

את המושג "משלבים מחרוזת" יזמו בשפת התכנות Perl, ומאז הוא מועבר לשפות אחרות. כמה חלופות למשלוב מחרוזות הן: משלוב בשפת המחשב C עם `sprintf()`, או בעזרת שימוש באופרטור '+' על מחרוזות. 

המימוש ב־ PowerShell מספק דרך קלה וברורה למשלוב מחרוזות תוך כדי שמירה על קריאות הקוד.

## ראו גם:

- [תיעוד של Microsoft על מחרוזות ב־ PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-string-substitutions?view=powershell-7.1)
- [פוסט בבלוג על משלוב מחרוזות ב־ PowerShell](https://devblogs.microsoft.com/scripting/weekend-scripter-use-powershell-to-easily-create-strings-from-variables/)
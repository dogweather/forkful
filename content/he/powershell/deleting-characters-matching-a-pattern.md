---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים שמתאימים לדפוס מסוים היא פעולה שבה משתמשים בביטויים רגולריים או באלגוריתמים אחרים כדי למחוק תווי ספציפיים מתוך מחרוזת. מתכנתים מבצעים זאת כדי לטהר נתונים, לשפר את הניתונים שלהם, ולאפשר ביצועים מועדפים.

## איך לעשות:
```PowerShell
# אנחנו נעבוד עם המחרוזת הבאה
$string = "Hello, עולם! אנחנו לומדים PowerShell 123."

# מחק תווים לפי דפוס מסוים באמצעות -Replace
$string = $string -Replace '[א-ת]','' # מחיקת תווים עבריים בלבד
$string = $string -Replace '[^a-zA-Z^]','' # מחיקת כל תו שאינו אות לטנית

# תוצאה
HelloPowerShell
```

## צלילה עמוקה:
המחיקה של תווים שמתפקדים לפי דפוס מסוים היא חלק בלתי נמנע מעיבוד מחרוזות מאז ששפת תכנות בראש פעם שוחררה. זה היה ונותר דרך משמעותית לניקוי וניפוי נתונים. Kotlin, Python, Java ואחרות, הן בין השפות שמאפשרות זאת גם כן. עם PowerShell, הטריק הוא להשתמש ב `-Replace`, שמאפשר לכם להחליף תווים שמתאימים לדפוס המבוקש. זו הסיבה שיש לנו את הצורך להחזיר מחרוזת ריקה '' כחלופה.

## ראו גם:
* [דוקומנטציה הרשמית של פעול Elace PowerShell R](https://docs.microsoft.com/he-il/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1#example-4--replace-operator)
* [הערכה מהירה של תוכנית סקריפטים בשפת PowerShell](https://stackoverflow.com/questions/8762406/how-can-i-remove-a-specific-character-from-a-string-in-powershell)
---
title:                "מחיקת תווים התואמים את הדפוס"
html_title:           "PowerShell: מחיקת תווים התואמים את הדפוס"
simple_title:         "מחיקת תווים התואמים את הדפוס"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

מה ולמה?

מחיקת תווים התואמים לתבנית היא תהליך שבו מחפשים ומוחקים תווים מסוימים בטקסט על סמך תבנית מסוימת. פעולה זו חשובה למתכנתים כדי לנקות טקסטים שנוצרו באופן מכולו.

כיצד לעשות זאת?

```PowerShell
# נמחק את התווים הנמצאים בין התו "" לתו ""
$str = "זה תוים ""לא נחמדים"" בטקסט שלנו"
$str -replace '".*?"', ""
# פלט: זה תוים בטקסט שלנו
```

```PowerShell
# נמחק את כל האותיות הקטנות מהמחרוזת
$str = "המחרוזת כל AMA נכתבת רק באותיות קטנות"
$str -replace "[a-z]", ""
# פלט: AAMA
```

צלילה עמוקה

מחיקת תווים התואמים לתבנית היא פונקציונליות שנמצאת מזה עשורים בשפות תכנות כמו Perl ו- awk. זה מאפשר למתכנת לבצע פעולות מורכבות על טקסט בצורה נוחה יותר, במיוחד כאשר מדובר בטקסטים גדולים.

אלטרנטיבות למחיקת תווים התואמים לתבנית כוללות את פקודת ```Select-String``` ב-PowerShell ופקודת ```grep``` בשפת Unix. בנוסף, ניתן לבצע פעולה דומה בעזרת פונקציות תכנותיות כגון ```substring``` בשפת C.

מימוש פנימי של מחיקת תווים התואמים לתבנית דורש שימוש בתבניות רגולריות למציאת התווים הרלוונטיים למחיקה וביצוע החלפה שלהם בשפה. זה משמש כמנגנון אחסון וטכנולוגיה יסודית ביותר ליישומים בתחום עיבוד הטקסט.

ראו גם

- [תיעוד רשמי על פקודת -replace ב-PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1#replace-operator)
- [מדריך רגולרי ל-PowerShell](https://www.regular-expressions.info/powershell.html)
- [Exploring Grep - פאנל מקומי של תהליך grepping](https://www.youtube.com/watch?v=QZ1blL9KjjI&t=5s)
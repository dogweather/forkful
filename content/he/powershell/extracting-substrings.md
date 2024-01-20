---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
חליצת תת-מחרוזות היא פעולת קיצור של מחרוזת מתחילתה או מסוף שלה. מתכנתים משתמשים בה כדי לקבל מידע מסוים ממחרוזת, או לשנות מידע באופן יעיל.

## איך לעשות:
שימוש פשוט ב- ```substring()``` ישאיר את המחלק הראשון של המחרודת:
```PowerShell
$string = "Hello, world!"
$string.substring(0, 5) 
```
ביצוע הקוד הזה יחזיר: ```Hello```

ניתן גם להשאיר את החלק האחרון של המחרוזת:
```PowerShell
$string = "Hello, world!"
$string.substring(7)
```
ביצוע הקוד הזה יחזיר: ```world!```

## עומק יותר:
הפעולה ```substring()``` מקורה ב-LISP, שם הוצאת תת-מחרוזת היתה אפשרית לפני יותר מ-60 שנה.
ישנן דרכים אחרות להוציא תת-מחרוזת ב-PowerShell, כולל שימוש ב- ```split()``` או ב- ```replace()```, אבל ```substring()``` היא הדרך הרגילה משום שהיא יעילה במיוחד.
קיצור מחרוזות משנה את סדר המחרוזת, אבל לא את הערך שלה. המחרוזת המקורית לא משתנה.

## בחנו גם:
דוגמאות ומדריכים נוספים ניתן למצוא ב:
- [MSDN: חליצת תת-מחרוזות ב-PowerShell](https://msdn.microsoft.com/en-us/powershell/reference/5.1/microsoft.powershell.core/about/about_operators#substring-operator)
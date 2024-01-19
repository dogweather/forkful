---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מציאת האורך של מחרוזת היא תהליך של כמה אותיות, מספרים, סימנים או תווים אחרים יש במחרוזת מסוימת. מתכנתים משתמשים בזה כדי לבצע בדיקות, לאבחן בעיות ולנתב את תנאי הקוד.

## איך עושים זאת:
```Bash
# מחרוזת
str="Hello, World!"

# מציאת אור׭ מחרוזת
len=${#str}

# הדפסת האורכים
echo "אורך המחרוזת הוא $len"
```

הפלט:
```Bash
אורך המחרוזת הוא 13
```

## בתוך קוד התוכנה:
מעשייה של מציאת אורך מחרוזת התחילה עם שפת תכנות עתיקה בשם FORTRAN. ב-Bash, אנחנו משתמשים ב${#string} מדי פעם, והיא גם תכליתית ומהירה. ‏
חלופות נוספות הם שימוש ב- awk או expr, אך הרבה פעמים הן מעט יותר איטיות ומסורבלות.
המידע על האורך של מחרוזת מאוחסן בזיכרון המחשב, ולכן הפקתו היא תהליך מהיר ויעיל 

## עיין גם:
- [מדריך של GNU Bash](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- [למד על שפת תכנות Bash בוויקיפדיה](https://he.wikipedia.org/wiki/Bash)
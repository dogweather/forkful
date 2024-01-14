---
title:                "Bash: המרת מחרוזת לאותיות קטנות"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה

למה מישהו יעשה כיצד כדי להמיר מחרוזת לאותיות קטנות: כי זה כלי שימושי כאשר מתעסקים עם מחרוזות גדולות או כאשר יש להשוות מחרוזות באותו רמת הכתיב.

## איך לעשות

```Bash
# קוד דוגמא:
string="HELLO WORLD"
lower_case_string=${string,,}
echo $lower_case_string

# פלט: hello world
```

הפעולה הראשונה היא להגדיר משתנה עם המחרוזת המקורית. לאחר מכן, באמצעות התווים " ,, " נדפיס את משתנה המחרוזת באותיות קטנות. לבסוף, נדפיס את המחרוזת החדשה הנמצאת במשתנה החדש.

## Deep Dive

המרת מחרוזת לאותיות קטנות היא פעולה פשוטה, אך חשוב להבין שאת התהליך ניתן לבצע בכמה דרכים שונות בתכנות ה- Bash. בנוסף, כדי להתאים את הפעולה לצרכי המשתמש, ניתן להשתמש בפונקציות נוספות כמו " tr" או " awk". כמו כן, חשוב לציין שהמחרוזת המקורית תישאר ללא שינויים, כלומר המרת האותיות לאותיות קטנות תתבצע רק על המילים ה"תחתונות" במחרוזת.

## ראה גם

- [מדריך לפונקציות בתכנות ה- Bash](https://geek.co.il/2015/05/28/bash-shell-scripting-functions)
- [מדריך לשימוש בפקודת " tr" עבור עיבוד מחרוזות](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [מדריך לשימוש בתוכניות של " awk" ב- Bash](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
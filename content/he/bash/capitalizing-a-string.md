---
title:                "הפיכת מחרוזת לאותיות רישיות"
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? / מה ולמה?
בשינוי אותיות לרישיות, אנחנו משנים את ראשי התיבות של מחרוזת לאותיות גדולות. פרוגרמרים עושים זאת לשם קביעת תצורה, כמו בכותרות, או לשימוש עקבי במשתנים ופונקציות.

## How to: / איך לעשות:
עם Bash, אתה יכול לקפיטליזציה מחרוזת בקלות. כאן כמה דוגמאות:

```Bash
#!/bin/bash
# Capitalizing a single word
word="bash"
echo "${word^}"
# Output: Bash

# Capitalizing the entire string
entire_string="hello, world!"
echo "${entire_string^^}"
# Output: HELLO, WORLD!

# Capitalize first letter of each word
for word in $entire_string; do 
    echo -n "${word^} "
done
# Output: Hello, World! 
```

## Deep Dive / עומק הדברים:
מאז שינויים חדשים ב-Bash בגרסא 4.0 (2009), התווספה האפשרות לשינוי רישיות מחרוזות וכך לא נדרש להשתמש בעזרים חיצוניים כמו `tr` או `awk`. הוספת הפונקציונליות נעשתה בשימוש בקודי מעטפת מרובים (brace expansions). האות `^` משמשת להפיכת האות הראשונה לרישית, והסימן `^^` לכל האותיות במחרוזת. קיימות גם הפכים `,` ו-`,,` להפיכה לאותיות קטנות. יש גם אלטרנטיבות כמו הפעלת `sed` או כתיבת סקריפט ב-Python.

## See Also / ראה גם:
- תיעוד של Bash: https://www.gnu.org/software/bash/manual/
- קורס בחינם ללמידת Bash: https://www.codecademy.com/learn/learn-the-command-line
- מדריך מפורט על brace expansions ב-Bash: https://www.gnu.org/software/bash/manual/html_node/Brace-Expansion.html
- מאמרים על שימושים מתקדמים של `sed` ו`awk`: `http://www.grymoire.com/Unix/Sed.html` ו`http://www.grymoire.com/Unix/Awk.html`
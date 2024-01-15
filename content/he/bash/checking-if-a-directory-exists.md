---
title:                "לבדיקת קיום תיקייה"
html_title:           "Bash: לבדיקת קיום תיקייה"
simple_title:         "לבדיקת קיום תיקייה"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## על מה
הצורך לבדוק אם תיקייה קיימת יכול להיות בשימוש במגוון מקרים, כגון יצירת תנאים בתוכניות, ניטור קבצים או ביצוע סקריפטים שונים.

## כיצד לבדוק אם תיקייה קיימת
שימוש בפקודת `test` עם הפרמטר `-d` עבור התיקייה המבוקשת תאפשר לנו לבדוק אם התיקייה קיימת. לדוגמה:

```Bash
test -d /home/user/Documents
```

אם התיקייה קיימת, יתקבל פלט של `0`, או אם התיקייה לא קיימת, יתקבל פלט של `1`.

## העיון העמוק
בנוסף לפקודת `test`, ניתן גם להשתמש בפקודה `[[` ולרשום את התנאי המבוקש בסוגריים. למשל:

```Bash
if [[ -d /home/user/Documents ]]; then
  echo "The directory exists!"
fi
```

כמו כן, ניתן גם להשתמש בפקודת `ls` ולבדוק את הפלט שלה כדי לראות אם התיקייה קיימת. למשל:

```Bash
ls /home/user/Documents 2> /dev/null
if [[ $? -eq 0 ]]; then
  echo "The directory exists!"
fi
```

## ראו גם
- [פקודת test](https://www.gnu.org/software/coreutils/manual/html_node/test-invocation.html#test-invocation)
- [פקודת [[ ותנאיים ב-Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
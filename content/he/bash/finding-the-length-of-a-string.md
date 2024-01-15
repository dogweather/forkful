---
title:                "מציאת אורך מחרוזת"
html_title:           "Bash: מציאת אורך מחרוזת"
simple_title:         "מציאת אורך מחרוזת"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## למה
כדי לדעת את אורך הטקסט או המחרוזת המסוימת שמעוניינים לעבוד איתה.

## כיצד לעשות זאת
```Bash
# קוד לאיתור אורך המחרוזת או הטקסט
text="שלום עולם"
echo ${#text} # פלט: 11 
```

ניתן להשתמש בפקודה echo יחד עם תווי ה- # כדי להדפיס את אורך המחרוזת או הטקסט הנתון. בדוגמא שלנו, נגיד שיש לנו מחרוזת המכילה את המילה "שלום עולם", נרצה לדעת את אורךה כדי לעבוד עם זה. בשורות הקוד לעיל, אנו מגדירים תחילה את המחרוזת הרצויה תחת המשתנה "text" ואז משתמשים בתווי ה- # כדי להדפיס את אורך המחרוזת.

## חפירה עמוקה
ב-Bash, ניתן להשתמש בפקודה "expr" כדי למצוא את אורך המחרוזת או הטקסט המבוקש. למשל, אם נרצה להציג את אורך המחרוזת "hello", נוכל לכתוב את הקוד הבא:

```Bash
eval("expr length "hello"")
```

כאן, אנו משתמשים בפקודה "eval" כדי לרוץ את פקודת ה-"expr" ואז שם את המחרוזת "hello" בנקודת התחלת הפקודה. פקודת ה-"expr" תחזיר את אורך המחרוזת (5 במקרה שלנו) ויהיה פלט המוצג למסך.

## ראה גם
- [התחלה מהירה למודד הפקודות בBash](https://linuxize.com/post/how-to-measure-the-execution-time-of-a-command-in-linux/)
- [תיעוד מעמיק לפקודת "expr"](https://www.geeksforgeeks.org/expr-command-in-linux-with-examples/)
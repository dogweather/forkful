---
title:                "המרת מחרוזת לאותיות קטנות"
aliases:
- /he/bash/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:29.167373-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות היא פעולה בה אתה משנה את האותיות הגדולות במחרוזת לאותיות קטנות. תכנותים עושים את זה ליצירת אחידות, להשוואת טקסטים, ולטפל בקלט שמשתנה ברגישיות לרישיות.

## איך לעשות זאת:
ב-Bash, המרת מחרוזת לאותיות קטנות פשוטה. נתחיל בדוגמה פשוטה עם `tr`:

```Bash
echo "Hello World!" | tr '[:upper:]' '[:lower:]'
```
פלט:
```
hello world!
```

עוד דרך היא להשתמש ב-`${parameter,,}` syntax של Bash מגרסה 4 ומעלה:

```Bash
str="Hello Again!"
echo "${str,,}"
```
פלט:
```
hello again!
```

דוגמה ל-loop הממיר את כל המחרוזות במערך:

```Bash
declare -a arr=("One" "Two" "Three")
for i in "${arr[@]}"; do
  echo "${i,,}"
done
```
פלט:
```
one
two
three
```

## צלילה עמוקה:
ב-Bash, מנגנוני המרה לאותיות קטנות מתפתחים במהלך הזמן. במקור, עשו את זה בעיקר עם `tr` או `awk`. מאז גרסה 4 של באש, הוכנסה תחביר של expansion למחרוזת שמאפשר מניפולציות גודל. הדרך הזו נחשבת יותר יעילה ביחס לפעולות קלות ונוחה יותר לקריאה. 

גם על פי ביצועים, לכל פקודה יתרונות וחסרונות במקרים שונים. כשמעבדים טקסט גדול מאוד, שיקולים של מהירות ושימוש בזיכרון יכולים להיות חשובים, ולעתים עשויים לדרוש בחינה והשוואה בין שיטות שונות.

## ראה גם:
- [מדריך כתיבת סקריפטים ב-Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [תיעוד GNU לפקודת tr](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [דיסקוסיה על שיטות המרה בפורום Stack Overflow](https://stackoverflow.com/questions/2264428/how-to-convert-a-string-to-lower-case-in-bash)

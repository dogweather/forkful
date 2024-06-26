---
date: 2024-01-20 17:38:29.167373-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05D1-Bash, \u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05E4\u05E9\
  \u05D5\u05D8\u05D4. \u05E0\u05EA\u05D7\u05D9\u05DC \u05D1\u05D3\u05D5\u05D2\u05DE\
  \u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4 \u05E2\u05DD `tr`."
lastmod: '2024-03-13T22:44:39.599201-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-Bash, \u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05E4\
  \u05E9\u05D5\u05D8\u05D4."
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
weight: 4
---

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

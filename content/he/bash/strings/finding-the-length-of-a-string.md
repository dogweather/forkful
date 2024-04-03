---
date: 2024-01-20 17:47:22.600927-07:00
description: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05E9\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\
  \u05DE\u05D3\u05D5\u05D3 \u05DB\u05DE\u05D4 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D9\
  \u05E9 \u05D1\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\
  \u05D0 \u05E7\u05DC\u05D8, \u05EA\u05E7\u05E9\u05D5\u05E8\u05EA \u05D1\u05D9\u05DF\
  \ \u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD, \u05D5\u05E2\u05D5\u05D3."
lastmod: '2024-03-13T22:44:39.605826-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05E9\u05DC \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05DE\
  \u05D3\u05D5\u05D3 \u05DB\u05DE\u05D4 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D9\u05E9\
  \ \u05D1\u05D4."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
weight: 7
---

## מה ולמה?
מציאת אורך של מחרוזת זה פשוט למדוד כמה תווים יש בה. מתכנתים עושים את זה כדי לוודא קלט, תקשורת בין סקריפטים, ועוד.

## איך עושים את זה:
מצא את האורך על ידי שימוש ב-`${#string}`.
```Bash
my_string="שלום, עולם!"
echo ${#my_string}
```
תוצאה:
```
12
```

ניתן גם להשתמש ב-`expr`:
```Bash
my_string="שלום, עולם!"
length=$(expr length "$my_string")
echo $length
```
תוצאה:
```
12
```

## עיון מעמיק
העקרון של מציאת אורך מחרוזת הוא קדום כמעט כמו המחשוב עצמו. בעבר, שפות תכנות פחות גמישות היו דורשות לולאות מורכבות למטרה זו.

כמה אלטרנטיבות ב-Bash:
- `wc -m <<< "$my_string"` יחזיר את מספר תווים כולל סיומת שורה.
- תוספת של Unicode ותווים מיוחדים הופכת את בדיקת אורך למשימה לא פשוטה תמיד; חלק מהתווים יכולים להתפרש כמספר דמויות.

פירוט יישום ב-Bash:
- `${#string}` היא הדרך המהירה והפשוטה ביותר.
- פקודת `expr length` נחשבת יותר נוסטלגית ובעלת תוצאה דומה.
- Bash אינו בהכרח טוב בטיפול במחרוזות Unicode, לכן לעיתים פעולות על מחרוזות מורכבות ידרשו כלים חיצוניים או תוספות בשפה.

## ראה גם

- [Bash Advanced Scripting Guide](https://tldp.org/LDP/abs/html/)
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [מדריך לגבי שימוש במחרוזות ב-Bash](https://mywiki.wooledge.org/BashGuide/Parameters#Strings)

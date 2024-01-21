---
title:                "מציאת אורך מחרוזת"
date:                  2024-01-20T17:47:22.600927-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

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
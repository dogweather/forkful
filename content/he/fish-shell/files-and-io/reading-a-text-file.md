---
date: 2024-01-20 17:54:38.064774-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05E7\u05D7\u05EA \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05EA\u05D5\u05DA \u05E7\u05D5\u05D1\u05E5\
  \ \u05E9\u05DE\u05D7\u05D6\u05D9\u05E7 \u05D8\u05E7\u05E1\u05D8. \u05EA\u05DB\u05E0\
  \u05D5\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4\
  \ \u05DB\u05D9 \u05D0\u05D6 \u05D4\u05DD \u05D9\u05DB\u05D5\u05DC\u05D9\u05DD \u05DC\
  \u05E2\u05D1\u05D3, \u05DC\u05E0\u05EA\u05D7, \u05D0\u05D5 \u05DC\u05D4\u05E6\u05D9\
  \u05D2 \u05DE\u05D9\u05D3\u05E2 \u05D7\u05E9\u05D5\u05D1."
lastmod: '2024-03-13T22:44:40.078999-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05E7\u05D7\u05EA \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05DE\u05EA\u05D5\u05DA \u05E7\u05D5\u05D1\u05E5 \u05E9\
  \u05DE\u05D7\u05D6\u05D9\u05E7 \u05D8\u05E7\u05E1\u05D8."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 22
---

## מה ולמה?
קריאת קובץ טקסט זה פשוט לקחת נתונים מתוך קובץ שמחזיק טקסט. תכנותים עושים את זה כי אז הם יכולים לעבד, לנתח, או להציג מידע חשוב.

## איך לעשות:
קריאת קובץ ב-Fish שפשוטה וישירה. איך עושים את זה? ראו קוד:

```Fish Shell
# כדי לקרוא את התוכן של קובץ טקסט:
cat file.txt

# או, אם רוצים לקרוא בצורה מתוחכמת יותר:
string match -r '.*' < file.txt
```

פלט דוגמה:

```
> cat file.txt
שלום, עולם! זהו תוכן הקובץ.
```

## טבילת אש:
היכולת לקרוא קבצים זו בסיס לשפת תכנות. זה אחד מהמינימומים ששפת תכנות צריכה לתמוך. מעבר ל-`cat` ו-`string match`, יש גם כלים כמו `sed`, `awk`, `grep`, אשר מספקים יכולות עיבוד רבות ומורכבות יותר. ב-Fish, גם ניתן לשמור את התוכן של קובץ במשתנה ולעבוד איתו בצורה יותר נוחה.

```Fish Shell
# קרא ושמור במשתנה
set file_content (cat file.txt)

# ואז ניתן לעבד את הנתונים במשתנה
echo $file_content | string replace 'עולם' 'World'
```

פלט דוגמה:

```
> echo $file_content | string replace 'עולם' 'World'
שלום, World! זהו תוכן הקובץ.
```

## ראו גם:
- [התיעוד הראשי של Fish](https://fishshell.com/docs/current/index.html)
- [סדרת מדריכים ל-Fish](https://fishshell.com/docs/current/tutorial.html)
- [מעבדת תחביר הוראות Fish](https://fishshell.com/docs/current/cmds/read.html)

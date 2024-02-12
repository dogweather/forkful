---
title:                "קריאת קובץ טקסט"
aliases:
- /he/fish-shell/reading-a-text-file.md
date:                  2024-01-20T17:54:38.064774-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

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

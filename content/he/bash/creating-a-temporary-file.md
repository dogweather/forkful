---
title:                "יצירת קובץ זמני"
aliases:
- he/bash/creating-a-temporary-file.md
date:                  2024-01-20T17:40:33.349490-07:00
model:                 gpt-4-1106-preview
simple_title:         "יצירת קובץ זמני"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני ב-Bash היא דרך לשמור נתונים או להפעיל פקודות כשאתה לא רוצה לזרוק את המידע ישר על הדיסק. פרוגרמיסטים עושים את זה כדי להבטיח כי המידע לא יישמר לטווח ארוך ולא יתערבב עם קבצים אחרים.

## איך לעשות:
קודים עם דוגמאות ופלטים בבלוקים של קוד.

```Bash
# יצירת קובץ זמני
tempfile=$(mktemp)

# בדיקה שהקובץ נוצר
ls -l $tempfile

# שימוש בקובץ, לדוגמה - כתיבה אליו
echo "Data for the temporary file" > $tempfile

# קריאה מהקובץ
cat $tempfile

# מחיקת הקובץ כשאנחנו סיימנו איתו
rm $tempfile
```

## לעומק:
פעם, פרוגרמיסטים היו יוצרים קבצים זמניים ידנית וקוראים להם עם שמות כמו `tmp` או `temp`. הבעיה הייתה שזה עלול להביא להתנגשויות ובעיות אבטחה. הפקודה `mktemp` נוצרה כדי לפתור את זה, יוצרת קובץ עם שם ייחודי שקשה לנחש. יש גם אלטרנטיבות כמו `tempfile` (שאינה נתמכת יותר) ושימוש ב-`/dev/shm` במערכות הפעלה שתומכות בזה.

פקודת `mktemp` מאפשרת לך ליצור גם ספריות זמניות על ידי שימוש באפשרות `mktemp -d`, מה ששימושי כשאתה צריך לעבוד עם מספר קבצים זמניים יחד. ביטחון אוהב את `mktemp` מכיוון שהוא מונע קונפליקטים מרובים תהליכים ופוטנציאל להתקפות.

## ראה גם:

- [`mktemp` man page](https://man7.org/linux/man-pages/man1/mktemp.1.html) – מדריך מפורט ל-pktemp, כולל אופציות.
- Advanced Bash-Scripting Guide: [Chapter 19. Temporary Files](https://tldp.org/LDP/abs/html/tempfiles.html) – למידע נוסף על קבצים זמניים ואיך להשתמש בהם בסקריפטים.
- Safe File and Variable Names: [Common Pitfalls](https://dwheeler.com/essays/filenames-in-shell.html) – הסבר על שמות קבצים בקונטקסט של סקריפטים ואיך להימנע מצרות.

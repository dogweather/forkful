---
date: 2024-01-20 17:40:33.349490-07:00
description: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\
  \u05E0\u05D9 \u05D1-Bash \u05D4\u05D9\u05D0 \u05D3\u05E8\u05DA \u05DC\u05E9\u05DE\
  \u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05DC\u05D4\u05E4\
  \u05E2\u05D9\u05DC \u05E4\u05E7\u05D5\u05D3\u05D5\u05EA \u05DB\u05E9\u05D0\u05EA\
  \u05D4 \u05DC\u05D0 \u05E8\u05D5\u05E6\u05D4 \u05DC\u05D6\u05E8\u05D5\u05E7 \u05D0\
  \u05EA \u05D4\u05DE\u05D9\u05D3\u05E2 \u05D9\u05E9\u05E8 \u05E2\u05DC \u05D4\u05D3\
  \u05D9\u05E1\u05E7. \u05E4\u05E8\u05D5\u05D2\u05E8\u05DE\u05D9\u05E1\u05D8\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9\
  \ \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7 \u05DB\u05D9 \u05D4\u05DE\u05D9\u05D3\u05E2\
  \ \u05DC\u05D0\u2026"
lastmod: '2024-03-13T22:44:39.656918-06:00'
model: gpt-4-1106-preview
summary: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9 \u05D1-Bash \u05D4\u05D9\u05D0 \u05D3\u05E8\u05DA \u05DC\u05E9\u05DE\u05D5\
  \u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05DC\u05D4\u05E4\u05E2\
  \u05D9\u05DC \u05E4\u05E7\u05D5\u05D3\u05D5\u05EA \u05DB\u05E9\u05D0\u05EA\u05D4\
  \ \u05DC\u05D0 \u05E8\u05D5\u05E6\u05D4 \u05DC\u05D6\u05E8\u05D5\u05E7 \u05D0\u05EA\
  \ \u05D4\u05DE\u05D9\u05D3\u05E2 \u05D9\u05E9\u05E8 \u05E2\u05DC \u05D4\u05D3\u05D9\
  \u05E1\u05E7."
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
weight: 21
---

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

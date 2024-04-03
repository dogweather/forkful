---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:17.263965-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05DC\u05D9\
  \u05D1\u05D5 \u05E9\u05DC \u05D4\u05E2\u05E0\u05D9\u05D9\u05DF, Bash \u05DE\u05D0\
  \u05E4\u05E9\u05E8 \u05DC\u05DA \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D0\u05EA \u05E7\
  \u05D9\u05D5\u05DE\u05D4 \u05E9\u05DC \u05EA\u05D9\u05E7\u05D9\u05D9\u05D4 \u05D1\
  \u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05DE\u05E9\u05E4\u05D8\u05D9\u05DD \u05EA\
  \u05E0\u05D0\u05D9\u05D9\u05DD \u05D5\u05D0\u05D5\u05E4\u05E8\u05D8\u05D5\u05E8\
  \ `-d`. \u05DC\u05D4\u05DC\u05DF \u05D3\u05D5\u05D2\u05DE\u05D0 \u05E4\u05E9\u05D5\
  \u05D8\u05D4 \u05D4\u05DE\u05D3\u05D2\u05D9\u05DE\u05D4 \u05D0\u05D9\u05DA \u05DC\
  \u05D1\u05E6\u05E2 \u05D1\u05D3\u05D9\u05E7\u05D4 \u05D6\u05D5."
lastmod: '2024-03-13T22:44:39.648846-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05DC\u05D9\u05D1\u05D5 \u05E9\u05DC \u05D4\u05E2\u05E0\u05D9\u05D9\
  \u05DF, Bash \u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DA \u05DC\u05D1\u05D3\u05D5\
  \u05E7 \u05D0\u05EA \u05E7\u05D9\u05D5\u05DE\u05D4 \u05E9\u05DC \u05EA\u05D9\u05E7\
  \u05D9\u05D9\u05D4 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05DE\u05E9\u05E4\
  \u05D8\u05D9\u05DD \u05EA\u05E0\u05D0\u05D9\u05D9\u05DD \u05D5\u05D0\u05D5\u05E4\
  \u05E8\u05D8\u05D5\u05E8 `-d`."
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
weight: 20
---

## איך לעשות:
בליבו של העניין, Bash מאפשר לך לבדוק את קיומה של תיקייה באמצעות משפטים תנאיים ואופרטור `-d`. להלן דוגמא פשוטה המדגימה איך לבצע בדיקה זו.

```bash
if [ -d "/path/to/directory" ]; then
    echo "התיקייה קיימת."
else
    echo "התיקייה לא קיימת."
fi
```

פלט לדוגמה (אם התיקייה קיימת):
```
התיקייה קיימת.
```

פלט לדוגמה (אם התיקייה לא קיימת):
```
התיקייה לא קיימת.
```

לסקריפטים מורכבים יותר, נהוג לשלב את הבדיקה עם פעולות נוספות, כגון יצירת התיקייה אם היא לא קיימת:

```bash
DIR="/path/to/directory"
if [ -d "$DIR" ]; then
    echo "$DIR קיימת."
else
    echo "$DIR לא קיימת. יוצר עכשיו..."
    mkdir -p "$DIR"
    echo "$DIR נוצרה."
fi
```

פלט לדוגמה (אם התיקייה לא קיימת ואז נוצרת):
```
/path/to/directory לא קיימת. יוצר עכשיו...
/path/to/directory נוצרה.
```

למרות ש-Bash עצמו מספק כלים עמידים לבדיקות כאלה, אין ספריות צד שלישי פופולריות במיוחד למשימה זו, מאחר ופקודות Bash טבעיות כבר יעילות ומסוגלות לחלוטין לאימות נוכחות של תיקיות.

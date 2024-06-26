---
date: 2024-01-26 04:11:55.691770-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Bash, \u05D4\
  \u05D8\u05E8\u05DE\u05D9\u05E0\u05DC \u05E9\u05DC\u05DA \u05D4\u05D5\u05D0 \u05DC\
  \u05DE\u05E2\u05E9\u05D4 REPL. \u05D0\u05EA\u05D4 \u05DE\u05E7\u05DC\u05D9\u05D3\
  \ \u05E4\u05E7\u05D5\u05D3\u05D4; \u05D4\u05D9\u05D0 \u05E7\u05D5\u05E8\u05D0\u05EA\
  \ \u05D0\u05D5\u05EA\u05D4, \u05DE\u05E2\u05E8\u05D9\u05DB\u05D4 \u05D0\u05D5\u05EA\
  \u05D4, \u05DE\u05D3\u05E4\u05D9\u05E1\u05D4 \u05D0\u05EA \u05D4\u05EA\u05D5\u05E6\
  \u05D0\u05D4, \u05D5\u05D7\u05D5\u05D6\u05E8\u05EA \u05DC\u05D4\u05DE\u05EA\u05D9\
  \u05DF \u05DC\u05E4\u05E7\u05D5\u05D3\u05D4 \u05D4\u05D1\u05D0\u05D4 \u05E9\u05DC\
  \u05DA. \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4\u2026"
lastmod: '2024-03-13T22:44:39.627285-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Bash, \u05D4\u05D8\u05E8\u05DE\u05D9\u05E0\u05DC \u05E9\u05DC\u05DA\
  \ \u05D4\u05D5\u05D0 \u05DC\u05DE\u05E2\u05E9\u05D4 REPL."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
weight: 34
---

## איך לעשות:
ב-Bash, הטרמינל שלך הוא למעשה REPL. אתה מקליד פקודה; היא קוראת אותה, מעריכה אותה, מדפיסה את התוצאה, וחוזרת להמתין לפקודה הבאה שלך. הנה דוגמה לשימוש ב-Bash כ-REPL:

```Bash
$ echo "שלום, עולם!"
שלום, עולם!
$ x=$((6 * 7))
$ echo $x
42
```

הקלט שלך מתחיל אחרי הסימן `$ `, והפלט מודפס בשורה הבאה. פשוט, נכון?

## צלילה עמוקה
Bash, שהמשמעות שלו היא Bourne Again SHell, הוא ה-shell המוגדר כברירת מחדל על מערכות מבוססות Unix רבות. זו שדרוג ל-shell המקורי של Bourne, שנבנה בסוף שנות ה-70. למרות ש-Bash הוא כלי סקריפטינג עוצמתי, המצב האינטראקטיבי שלו מאפשר לך לבצע פקודות שורה אחר שורה.

כאשר מחפשים חלופות, יש לך את REPL של Python (פשוט כתוב `python` בטרמינל שלך), Node.js (עם `node`), ו-IPython, שלל Python אינטראקטיבי משופר. כל שפה נוטה להציע את היישום ה-REPL שלה.

מתחת לכל אלה, ה-REPLs הם לולאות שמנתחות את הקלט שלך (פקודות או קוד), מריצות אותו, ומחזירות את התוצאה ל-stdout (המסך שלך), לעיתים קרובות תוך שימוש ישיר במתורגמן של השפה. זו מיידיות של משוב היא מעולה ללמידה ולפרוטוטיפינג.

## ראה גם
- [תיעוד GNU Bash הרשמי](https://gnu.org/software/bash/manual/bash.html)
- [למד Shell באופן אינטראקטיבי](https://www.learnshell.org/)
- [אתר הרשמי של IPython](https://ipython.org/)
- [REPL.it](https://replit.com/): REPL אינטרנטי רב-שפתי (לא רק Bash!)

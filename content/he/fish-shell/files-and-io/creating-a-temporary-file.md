---
date: 2024-01-20 17:40:40.335476-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Fish Shell,\
  \ \u05D0\u05E4\u05E9\u05E8 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05E7\u05D5\u05D1\u05E5\
  \ \u05D6\u05DE\u05E0\u05D9 \u05D1\u05E7\u05DC\u05D5\u05EA \u05E2\u05DD \u05D4\u05E4\
  \u05E7\u05D5\u05D3\u05D4 `mktemp`. \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4\
  ."
lastmod: '2024-03-13T22:44:40.082345-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-Fish Shell, \u05D0\u05E4\u05E9\u05E8 \u05DC\u05D9\u05E6\u05D5\u05E8\
  \ \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\u05D9 \u05D1\u05E7\u05DC\u05D5\u05EA\
  \ \u05E2\u05DD \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4 `mktemp`."
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
weight: 21
---

## איך לעשות:
ב-Fish Shell, אפשר ליצור קובץ זמני בקלות עם הפקודה `mktemp`. הנה דוגמה:

```fish
set tempfile (mktemp)
echo "כאן יש תוכן זמני" > $tempfile
cat $tempfile
rm $tempfile
```

הפלט יוצא:

```
כאן יש תוכן זמני
```

ולאחר מכן הקובץ נמחק.

## ניתוח עמוק:
בעבר, קבצים זמניים נוצרו ידנית, שם המשתמש צריך היה לדאוג לא לדרוס קבצים קיימים ולנהל את שמות הקבצים. כלי כמו `mktemp` מסייע בפתרון הבעיות הללו על ידי יצירת שמות קבצים ייחודיים באופן אוטומטי. חלופות כוללות שימוש ב-UUID או בתאריכים ושעות לשמות הקבצים, אלו פחות מומלצות מכיוון שיש סיכוי קל (אם כי מאוד קטן) להתנגשות. `mktemp` בדרך כלל מייצרת קבצים בתיקיית `/tmp` במערכות Unix-דומות, תיקייה זו מיועדת לאחסון זמני ולעיתים נמחקת באיתחול המערכת או על ידי תוכנות ניקוי.

## ראה גם:
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html) - התיעוד הרשמי של Fish Shell, עם הסברים נוספים ואפשרויות שימוש.
- [man mktemp](https://linux.die.net/man/1/mktemp) - תיעוד על פקודת `mktemp` בלינוקס, שממנה Fish שאולה את התחביר.

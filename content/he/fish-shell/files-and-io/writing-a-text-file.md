---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:27.750624-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DB\u05D3\u05D9\
  \ \u05DC\u05DB\u05EA\u05D5\u05D1 \u05DC\u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D1-Fish, \u05EA\u05D5\u05DB\u05DC\u05D5 \u05DC\u05D4\u05E9\u05EA\u05DE\
  \u05E9 \u05D1\u05E4\u05E7\u05D5\u05D3\u05EA `echo` \u05D1\u05E9\u05D9\u05DC\u05D5\
  \u05D1 \u05E2\u05DD \u05D0\u05D5\u05E4\u05E8\u05D8\u05D5\u05E8\u05D9 \u05D4\u05DB\
  \u05D5\u05D5\u05E0\u05D4. \u05D0\u05D9\u05DF \u05E1\u05E4\u05E8\u05D9\u05D5\u05EA\
  \ \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05E4\u05D5\u05E4\u05D5\u05DC\u05E8\
  \u05D9\u05D5\u05EA \u05D1\u05DE\u05D9\u05D5\u05D7\u05D3 \u05DC\u05DB\u05EA\u05D9\
  \u05D1\u05EA \u05E7\u05D1\u05E6\u05D9\u05DD \u05D1-Fish,\u2026"
lastmod: '2024-03-13T22:44:40.080527-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05DB\u05EA\u05D5\u05D1 \u05DC\u05E7\u05D5\u05D1\
  \u05E5 \u05D8\u05E7\u05E1\u05D8 \u05D1-Fish, \u05EA\u05D5\u05DB\u05DC\u05D5 \u05DC\
  \u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E4\u05E7\u05D5\u05D3\u05EA `echo` \u05D1\
  \u05E9\u05D9\u05DC\u05D5\u05D1 \u05E2\u05DD \u05D0\u05D5\u05E4\u05E8\u05D8\u05D5\
  \u05E8\u05D9 \u05D4\u05DB\u05D5\u05D5\u05E0\u05D4."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 24
---

## איך לעשות:
כדי לכתוב לקובץ טקסט ב-Fish, תוכלו להשתמש בפקודת `echo` בשילוב עם אופרטורי הכוונה. אין ספריות צד שלישי פופולריות במיוחד לכתיבת קבצים ב-Fish, מכיוון שהפקודות המובנות של השל פשוטות ויעילות למטרה זו.

### כתיבת טקסט לקובץ חדש או דריסת קובץ קיים:
```fish
echo "שלום, Fish Shell!" > output.txt
```
פקודה זו כותבת "שלום, Fish Shell!" לתוך `output.txt`, יוצרת את הקובץ אם הוא לא קיים או דורסת אותו אם כן.

### צירוף טקסט לקובץ קיים:
אם ברצונכם להוסיף טקסט לסוף קובץ קיים מבלי להסיר את התוכן הנוכחי שלו, השתמשו באופרטור הוספה `>>`:
```fish
echo "מוסיף שורה חדשה לקובץ." >> output.txt
```

### כתיבת מספר שורות:
תוכלו לכתוב מספר שורות לקובץ על ידי שימוש ב-echo עם תו ירידת שורה `\n`, או שתוכלו לשרשר פקודות echo באמצעות נקודה פסיק:
```fish
echo "שורה ראשונה\nשורה שנייה" > output.txt
# או
echo "שורה ראשונה" > output.txt; echo "שורה שנייה" >> output.txt
```

### פלט לדוגמה:
כדי לצפות בתוכן של `output.txt` לאחר הרצת הפקודות לעיל, השתמשו בפקודת `cat`:
```fish
cat output.txt
```
```plaintext
שורה ראשונה
שורה שנייה
```
החלפה או הוספת טקסטים כפי שהוצג מתערבת בתוכן הקובץ לפי הצורך שלך, מדגימה דרכים פשוטות אך עוצמתיות לעבוד עם קבצי טקסט ב-Fish Shell.

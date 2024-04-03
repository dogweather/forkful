---
date: 2024-01-26 03:40:10.892065-07:00
description: "\u05D4\u05E1\u05E8\u05EA \u05E6\u05D9\u05D8\u05D5\u05D8\u05D9\u05DD\
  \ \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\
  \u05DC\u05D4 \u05E9\u05DC \u05D4\u05D5\u05E6\u05D0\u05EA \u05E1\u05D9\u05DE\u05E0\
  \u05D9 \u05D4\u05E6\u05D9\u05D8\u05D5\u05D8 \u05D4\u05DE\u05E7\u05D5\u05DC\u05E7\
  \u05DC\u05D9\u05DD \u05D4\u05D0\u05DC\u05D4, \u05E6\u05D9\u05D8\u05D5\u05D8 \u05D1\
  \u05D5\u05D3\u05D3 (' ') \u05D0\u05D5 \u05E6\u05D9\u05D8\u05D5\u05D8 \u05DB\u05E4\
  \u05D5\u05DC (\" \"), \u05DE\u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D4\u05D8\
  \u05E7\u05E1\u05D8\u05D5\u05D0\u05DC\u05D9\u05D9\u05DD \u05E9\u05DC\u05DA. \u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:40.025099-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E1\u05E8\u05EA \u05E6\u05D9\u05D8\u05D5\u05D8\u05D9\u05DD \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\
  \u05D4 \u05E9\u05DC \u05D4\u05D5\u05E6\u05D0\u05EA \u05E1\u05D9\u05DE\u05E0\u05D9\
  \ \u05D4\u05E6\u05D9\u05D8\u05D5\u05D8 \u05D4\u05DE\u05E7\u05D5\u05DC\u05E7\u05DC\
  \u05D9\u05DD \u05D4\u05D0\u05DC\u05D4, \u05E6\u05D9\u05D8\u05D5\u05D8 \u05D1\u05D5\
  \u05D3\u05D3 (' ') \u05D0\u05D5 \u05E6\u05D9\u05D8\u05D5\u05D8 \u05DB\u05E4\u05D5\
  \u05DC (\" \"), \u05DE\u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D4\u05D8\u05E7\
  \u05E1\u05D8\u05D5\u05D0\u05DC\u05D9\u05D9\u05DD \u05E9\u05DC\u05DA."
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 9
---

## מה ולמה?

הסרת ציטוטים ממחרוזת היא פעולה של הוצאת סימני הציטוט המקולקלים האלה, ציטוט בודד (' ') או ציטוט כפול (" "), מהנתונים הטקסטואליים שלך. תכנתים לעיתים עושים זאת כדי לנקות קלט או להכין נתונים לעיבוד נוסף ללא העומס של ציטוטים.

## איך לעשות:

ל-Fish יש כישוף מובנה לסוג זה של משימה. השתמש בפונקציה `string` בלי לזעות. בדוק את הכישופים האלה:

```fish
# דוגמה עם ציטוטים בודדים
set quoted "'Hello, World!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # פלט: Hello, World!

# אותו דבר עם ציטוטים כפולים
set double_quoted "\"Hello, Universe!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # פלט: Hello, Universe!
```

## צלילה לעומק

בימי ההיסטוריה הקמאית של שורת הפקודה, היית תופס מצב עם `sed` או `awk` כדי לנקוז ציטוטים; מערבולת אמיתית של קווי נטייה ודגלים מסתוריים. פונקציית `string` של Fish באה מתקופה חדשה יותר, והופכת את הקוד לנקי ואינטואיטיבי יותר.

אלטרנטיבות במעטפות פקודה אחרות עשויות עדיין להסתמך על כלים עתיקים אלו או להשתמש בשיטות מובנות משלהן כמו הרחבת פרמטרים ב-bash או מתאמים ב-zsh.

פונקציית `string` הולכת מעבר לניקוי ציטוטים. זהו סכין שוויצרי לפעולות מחרוזת ב-Fish. עם `string`, אתה יכול לחתוך, לפלט, לפצל, לחבר או אפילו להתאים באמצעות ביטויים רגולריים מחרוזות ישירות בטרמינל שלך.

## ראה גם

צלול עמוק יותר לתוך `string` בעזרת התיעוד הרשמי:
- [תיעוד מחרוזת של Fish Shell](https://fishshell.com/docs/current/commands.html#string)

לנוסטלגיה או כאשר כותבים סקריפטים עם מעטפות פקודה יותר מסורתיות, בדוק:
- [מדריך ל-Sed & Awk](https://www.grymoire.com/Unix/Sed.html)
- [הרחבת פרמטרים של Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)

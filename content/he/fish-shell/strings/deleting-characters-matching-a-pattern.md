---
date: 2024-01-20 17:42:24.890415-07:00
description: "\u05DB\u05D0\u05E9\u05E8 \u05D0\u05E0\u05D5 \u05DE\u05D5\u05D7\u05E7\
  \u05D9\u05DD \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\u05D5\u05D0\u05DE\u05D9\
  \u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA, \u05D0\u05E0\u05D5 \u05DC\u05DE\u05E2\
  \u05E9\u05D4 \u05DE\u05D4\u05E0\u05D3\u05E1\u05D9\u05DD \u05DE\u05D7\u05D3\u05E9\
  \ \u05D0\u05EA \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E9\u05DC\u05E0\u05D5\
  \ \u05DB\u05DA \u05E9\u05EA\u05E9\u05DE\u05D9\u05D8 \u05D0\u05EA \u05D4\u05D7\u05DC\
  \u05E7\u05D9\u05DD \u05E9\u05D0\u05D9\u05E0\u05E0\u05D5 \u05E8\u05D5\u05E6\u05D9\
  \u05DD. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E0\u05E7\u05D5\u05EA \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05E9\u05E0\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:40.018412-06:00'
model: gpt-4-1106-preview
summary: "\u05DB\u05D0\u05E9\u05E8 \u05D0\u05E0\u05D5 \u05DE\u05D5\u05D7\u05E7\u05D9\
  \u05DD \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\u05D5\u05D0\u05DE\u05D9\u05DD\
  \ \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA, \u05D0\u05E0\u05D5 \u05DC\u05DE\u05E2\u05E9\
  \u05D4 \u05DE\u05D4\u05E0\u05D3\u05E1\u05D9\u05DD \u05DE\u05D7\u05D3\u05E9 \u05D0\
  \u05EA \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E9\u05DC\u05E0\u05D5 \u05DB\
  \u05DA \u05E9\u05EA\u05E9\u05DE\u05D9\u05D8 \u05D0\u05EA \u05D4\u05D7\u05DC\u05E7\
  \u05D9\u05DD \u05E9\u05D0\u05D9\u05E0\u05E0\u05D5 \u05E8\u05D5\u05E6\u05D9\u05DD\
  . \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E0\u05E7\u05D5\u05EA \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD, \u05DC\u05E9\u05E0\u05D5\u05EA\u2026"
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
weight: 5
---

## מה ולמה?
כאשר אנו מוחקים תווים התואמים לתבנית, אנו למעשה מהנדסים מחדש את המחרוזת שלנו כך שתשמיט את החלקים שאיננו רוצים. תכניתנים עושים זאת כדי לנקות נתונים, לשנות פורמטים, ולהפוך טקסט לקל יותר לניתוח.

## איך ל:
ב-Fish Shell, אתה יכול להשתמש בפקודת `string match` ובאופרטורים כמו `string replace` כדי למחוק תווים שהתאימו לפטרן.

```Fish Shell
# שימוש ב-string replace למחיקת כל הספרות ממחרוזת
echo "abc123xyz" | string replace -r "[0-9]+" ""
```

פלט:
```
abcxyz
```

בדוגמה הזו, התווים "123" נמחקים מהמחרוזת.

## עיון מעמיק
מחיקת תווים בפטרן היא חלק מעקרונות תכנות יסודיים. בעבר, כלים כמו `sed` ו-`awk` ב-Unix היו תקניים למשימות אלה. Fish מאפשרת פתרון מודרני יותר עם תחביר פשוט יותר, דבר המשמעותי עבור תחזוקה ונגישות של סקריפטים. בעת השימוש ב-`string replace`, Fish תפעיל ביטויים רגולריים באמצעות הדגל `-r` לאיתור והחלפה של תחומי טקסט.

## ראו גם
- המדריך הרשמי של Fish לעבודה עם מחרוזות: https://fishshell.com/docs/current/commands.html#string
- מדריך לביטויים רגולריים (Regex): https://www.regular-expressions.info/
- פורום התמיכה של Fish: https://fishshell.com/docs/current/index.html#help

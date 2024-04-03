---
date: 2024-01-20 17:43:20.880207-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05D5\u05D0\
  \u05D5 \u05E0\u05E8\u05D0\u05D4 \u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05D1\u05E4\
  \u05E2\u05D5\u05DC\u05D4. \u05E0\u05E9\u05EA\u05DE\u05E9 \u05D1\u05DE\u05D5\u05D3\
  \u05D5\u05DC `re` \u05DC\u05D4\u05E1\u05E8\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD\
  \ \u05DC\u05E4\u05D9 \u05EA\u05D1\u05E0\u05D9\u05EA."
lastmod: '2024-03-13T22:44:38.610199-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05D5\u05D0\u05D5 \u05E0\u05E8\u05D0\u05D4 \u05E4\u05D9\u05D9\u05EA\
  \u05D5\u05DF \u05D1\u05E4\u05E2\u05D5\u05DC\u05D4."
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
weight: 5
---

## איך לעשות:
בואו נראה פייתון בפעולה. נשתמש במודול `re` להסרת תווים לפי תבנית.

```python
import re

# בואו נמחק את כל הספרות מהמחרוזת
original_string = "H3ll0 W0rld!"
clean_string = re.sub(r'\d', '', original_string) # \d מייצג כל ספרה
print(clean_string)
```

פלט:
```
Hll Wrld!
```
עכשיו, מה אם רוצים להסיר פסיקים ונקודות?

```python
punctuation_string = "Hello, World!"
no_punctuation = re.sub(r'[,.]', '', punctuation_string) # [,.] מייצג פסיק או נקודה
print(no_punctuation)
```

פלט:
```
Hello World!
```

## טבילה עמוקה
המודול `re` קיים בפייתון כבר שנים והוא סטנדרט לעבודה עם ביטויים רגולריים. הביטויים הרגולריים מאפשרים חיפוש והחלפה מתקדמת של טקסט לפי תבניות. אלטרנטיבה פשוטה ל`re` היא שימוש בשיטות הכלולות במחלקת המחרוזות של פייתון, כמו `replace()` ו`strip()`, אבל הם מוגבלים יותר. פרט היישום לכלל ההחלפה הזה האם הביטוי הבא: `re.sub(pattern, repl, string)`, כאשר `pattern` הוא התבנית הרצויה, `repl` הדבר להחליפה לתוך, ו`string` היא המחרוזת המקורית.

## ראה גם
- [מסמכי Python על ביטויים רגולריים](https://docs.python.org/3/library/re.html)
- [מדריך למתחילים בנושא ביטויים רגולריים](https://www.regular-expressions.info/)
- [Pythex: כלי אינטרנט לניסוי ביטויים רגולריים](https://pythex.org/)

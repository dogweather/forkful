---
title:                "מחיקת תווים התואמים לתבנית"
aliases:
- he/python/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:20.880207-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
הסרת תווים התואמים לתבנית היא פעולה שבה אתה מוחק מחרוזות שונות תווים מוגדרים. מתכנתים עושים זאת כדי לנקות נתונים, להכין מחרוזות לעיבוד נוסף, או להסיר תוכן לא רצוי.

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

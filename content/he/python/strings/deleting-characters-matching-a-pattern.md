---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D0\u05E0\u05D9\
  \ \u05E2\u05D5\u05E9\u05D4 \u05D6\u05D0\u05EA \u05DE\u05E1\u05E4\u05D9\u05E7 \u05DC\
  \u05E2\u05D9\u05EA\u05D9\u05DD \u05DB\u05DA \u05E9\u05E9\u05D9\u05E4\u05E6\u05E8\
  \u05EA\u05D9 \u05D0\u05D5\u05EA\u05D4 \u05DC\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\
  \u05D9\u05EA `delete()` \u05E4\u05E9\u05D5\u05D8\u05D4 \u05D6\u05D5. \u05D6\u05D4\
  \u05D5 \u05D2\u05DD \u05D4\u05D3\u05D2\u05DE\u05D4 \u05D8\u05D5\u05D1\u05D4 \u05E9\
  \u05DC\u2026"
lastmod: '2024-04-05T21:53:39.941534-06:00'
model: gpt-4-0125-preview
summary: "\u05D0\u05E0\u05D9 \u05E2\u05D5\u05E9\u05D4 \u05D6\u05D0\u05EA \u05DE\u05E1\
  \u05E4\u05D9\u05E7 \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05DB\u05DA \u05E9\u05E9\
  \u05D9\u05E4\u05E6\u05E8\u05EA\u05D9 \u05D0\u05D5\u05EA\u05D4 \u05DC\u05E4\u05D5\
  \u05E0\u05E7\u05E6\u05D9\u05D9\u05EA `delete()` \u05E4\u05E9\u05D5\u05D8\u05D4 \u05D6\
  \u05D5."
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\u05DE\
  \u05EA\u05D0\u05D9\u05DE\u05D9\u05DD \u05DC\u05D3\u05E4\u05D5\u05E1"
weight: 5
---

## איך לעשות:
```Python
import re

# טקסט לדוגמה
text = "Hello, World! 1234"

# הסרת כל הספרות
no_digits = re.sub(r'\d', '', text)
print(no_digits)  # פלט: "Hello, World! "

# הסרת פיסוק
no_punctuation = re.sub(r'[^\w\s]', '', text)
print(no_punctuation)  # פלט: "Hello World 1234"

# הסרת תנועות
no_vowels = re.sub(r'[aeiouAEIOU]', '', text)
print(no_vowels)  # פלט: "Hll, Wrld! 1234"
```

### הפונקציה המותאמת שלי

אני עושה זאת מספיק לעיתים כך ששיפצרתי אותה לפונקציית `delete()` פשוטה זו. זהו גם הדגמה טובה של [doctests](https://docs.python.org/3/library/doctest.html):

```python
def delete(string: str, regex: str) -> str:
    """
    >>> delete("Hello, world!", "l")
    'Heo, word!'

    >>> delete("Hello, world!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", string)
```



## עיון עמוק יותר
המנהג של מחיקת תווים התואמים לדפוס בטקסט יש שורשים עמוקים במדעי המחשב, שנעוץ חזרה לכלים מוקדמים של Unix כמו `sed` ו-`grep`. בPython, המודול `re` מספק את היכולת הזו, תוך שימוש בביטויים רגולריים - כלי חזק וגמיש לעיבוד טקסט.

חלופות למודול `re` כוללות:
- שיטות מחרוזת כמו `replace()` למקרים פשוטים.
- ספריות צד שלישי כמו `regex` לדפוסים מורכבים יותר ותמיכה טובה יותר בUnicode.

מאחורי הקלעים, כאשר אתה משתמש ב`re.sub()`, המפרש של Python מקמפל את הדפוס לסדרת bytecodes, שמעובדים על ידי מכונת מצב שבוצעת תיאום דפוסים ישירות על הטקסט הקלט. פעולה זו יכולה להיות משאב כבד למחרוזות גדולות או דפוסים מורכבים, לכן שיקולי ביצועים קריטיים לעיבוד נתונים גדולים.

## ראה גם
- [תיעוד המודול `re` של Python](https://docs.python.org/3/library/re.html): המסמכים הרשמיים לביטויים רגולריים בPython.
- [Regular-Expressions.info](https://www.regular-expressions.info/): מדריך מקיף לביטויים רגולריים.
- [מדריך של Real Python על regex](https://realpython.com/regex-python/): שימושים מעשיים של ביטויים רגולריים בPython.

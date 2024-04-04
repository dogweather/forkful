---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: ."
lastmod: '2024-04-04T01:28:02.136112-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05D3\u05E4\u05D5\u05E1"
weight: 5
---

## איך לעשות:
```Python
import re

# מחרוזת דוגמה
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

### פונקציה מותאמת שכתבתי

אני עושה את זה מספיק לעיתים כך ששיפצרתי את זה לפונקציית `delete()` הזו. זה גם הדגמה טובה של [doctests](https://docs.python.org/3/library/doctest.html):

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

## צלילה עמוקה
המנהג של מחיקת תווים התואמים לתבנית בטקסט יש שורשים עמוקים במדעי המחשב, חוזרים לכלים יוניקס מוקדמים כמו `sed` ו`grep`. בפייתון, המודול `re` מספק את היכולת הזו, נעזר בביטויים רגולריים – כלי עוצמתי וגמיש לעיבוד טקסט.

אלטרנטיבות למודול `re` כוללות:
- שיטות מחרוזת כמו `replace()` למקרים פשוטים.
- ספריות צד שלישי כמו `regex` לתבניות מורכבות יותר ותמיכה טובה יותר ביוניקוד.

מאחורי הקלעים, כאשר אתה משתמש ב`re.sub()`, המפרש של פייתון מקמפל את התבנית לסדרה של בייטקודים, המעובדים על ידי מכונת מצבים שמבצעת התאמת תבניות ישירות על הטקסט הקלט. הפעולה הזו יכולה להיות משאב יקר למחרוזות גדולות או תבניות מורכבות, לכן שיקולי ביצועים הם קריטיים לעיבוד נתונים גדולים.

## ראה גם
- [תיעוד המודול `re` של פייתון](https://docs.python.org/3/library/re.html): התיעוד הרשמי לביטויים רגולריים בפייתון.
- [Regular-Expressions.info](https://www.regular-expressions.info/): מדריך מקיף לביטויים רגולריים.
- [Real Python tutorial on regex](https://realpython.com/regex-python/): שימושים מהעולם האמיתי של ביטויים רגולריים בפייתון.

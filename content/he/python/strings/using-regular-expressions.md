---
aliases:
- /he/python/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:21.159297-07:00
description: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D4\u05DD \u05EA\u05D1\u05E0\u05D9\u05D5\u05EA \u05E9\
  \u05E0\u05DE\u05E6\u05D0\u05D5\u05EA \u05D1\u05E9\u05D9\u05DE\u05D5\u05E9 \u05DC\
  \u05D4\u05EA\u05D0\u05DE\u05EA \u05E9\u05D9\u05DC\u05D5\u05D1\u05D9 \u05EA\u05D5\
  \u05D5\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\
  \u05D4\u05DD \u05DC\u05D7\u05D9\u05E4\u05D5\u05E9, \u05E2\u05E8\u05D9\u05DB\u05D4\
  , \u05D0\u05D5 \u05E2\u05D9\u05D1\u05D5\u05D3 \u05D8\u05E7\u05E1\u05D8 \u05D1\u05D4\
  \u05EA\u05D1\u05E1\u05E1 \u05E2\u05DC \u05EA\u05D1\u05E0\u05D9\u05D5\u05EA \u05DE\
  \u05D5\u05D2\u05D3\u05E8\u05D5\u05EA,\u2026"
lastmod: 2024-02-18 23:08:52.426397
model: gpt-4-0125-preview
summary: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D4\u05DD \u05EA\u05D1\u05E0\u05D9\u05D5\u05EA \u05E9\
  \u05E0\u05DE\u05E6\u05D0\u05D5\u05EA \u05D1\u05E9\u05D9\u05DE\u05D5\u05E9 \u05DC\
  \u05D4\u05EA\u05D0\u05DE\u05EA \u05E9\u05D9\u05DC\u05D5\u05D1\u05D9 \u05EA\u05D5\
  \u05D5\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\
  \u05D4\u05DD \u05DC\u05D7\u05D9\u05E4\u05D5\u05E9, \u05E2\u05E8\u05D9\u05DB\u05D4\
  , \u05D0\u05D5 \u05E2\u05D9\u05D1\u05D5\u05D3 \u05D8\u05E7\u05E1\u05D8 \u05D1\u05D4\
  \u05EA\u05D1\u05E1\u05E1 \u05E2\u05DC \u05EA\u05D1\u05E0\u05D9\u05D5\u05EA \u05DE\
  \u05D5\u05D2\u05D3\u05E8\u05D5\u05EA,\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
ביטויים רגולריים (regex) הם תבניות שנמצאות בשימוש להתאמת שילובי תווים במחרוזות. מתכנתים משתמשים בהם לחיפוש, עריכה, או עיבוד טקסט בהתבסס על תבניות מוגדרות, מה שהופך אותם לנחוצים ביותר למשימות כמו אימות נתונים, פירוק, או המרה.

## איך ל:
השימוש בביטויים רגולריים בפייתון כרוך במודול `re`, שסופק קבוצה של פונקציות לעיבוד טקסט באמצעות ביטויים רגולריים.

### התאמת תבנית בסיסית
לחפש תבנית במחרוזת, השתמשו ב-`re.search()`. היא מחזירה אובייקט התאמה כאשר התבנית נמצאת, אחרת `None`.
```python
import re

text = "Learn Python programming"
match = re.search("Python", text)
if match:
    print("נמצאה תבנית!")
else:
    print("התבנית לא נמצאה.")
```
פלט:
```
נמצאה תבנית!
```

### קומפילציה של ביטויים רגולריים
לשימוש חוזר באותה תבנית, קומפלו אותה תחילה באמצעות `re.compile()` לביצועים טובים יותר.
```python
pattern = re.compile("Python")
match = pattern.search("Learn Python programming")
if match:
    print("נמצאה תבנית מקומפלת!")
```
פלט:
```
נמצאה תבנית מקומפלת!
```

### פיצול מחרוזות
לפצל מחרוזת בכל התאמה של תבנית regex, השתמשו ב-`re.split()`.
```python
result = re.split("\s", "Python is fun")
print(result)
```
פלט:
```
['Python', 'is', 'fun']
```

### מציאת כל ההתאמות
למצוא את כל המופעים שאינם מתנפחים של תבנית, השתמשו ב-`re.findall()`.
```python
matches = re.findall("n", "Python programming")
print(matches)
```
פלט:
```
['n', 'n']
```

### החלפת טקסט
השתמשו ב-`re.sub()` כדי להחליף הופעות של תבנית במחרוזת חדשה.
```python
replaced_text = re.sub("fun", "awesome", "Python is fun")
print(replaced_text)
```
פלט:
```
Python הוא מדהים
```

### ספריות צד שלישי
למרות שמודול ה-`re` המובנה של פייתון חזק, ספריות צד שלישי כמו `regex` מציעות יותר תכונות וביצועים משופרים. כדי להשתמש ב-`regex`, התקינו אותה דרך pip (`pip install regex`) וייבאו אותה בקוד שלכם.

```python
import regex

text = "Learning Python 3.8"
match = regex.search(r"Python\s(\d+\.\d+)", text)
if match:
    print(f"נמצאה גרסה: {match.group(1)}")
```
פלט:
```
נמצאה גרסה: 3.8
```

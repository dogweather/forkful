---
date: 2024-01-26 04:17:49.550302-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05E7\u05E4\u05E6\u05D5 \u05D9\u05E9\u05D9\
  \u05E8\u05D5\u05EA \u05D0\u05DC REPL \u05E9\u05DC \u05E4\u05D9\u05D9\u05EA\u05D5\
  \u05DF \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D4\u05E7\u05DC\u05D3\u05EA `python` \u05D1\
  \u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4 \u05E9\u05DC\u05DB\
  \u05DD. \u05DE\u05E9\u05DD, \u05D1\u05D3\u05E7\u05D5 \u05E4\u05E2\u05D5\u05DC\u05D5\
  \u05EA \u05E4\u05E9\u05D5\u05D8\u05D5\u05EA \u05D0\u05D5 \u05E7\u05D5\u05D3 \u05DE\
  \u05E8\u05D5\u05D1\u05D4 \u05E9\u05D5\u05E8\u05D5\u05EA."
lastmod: '2024-03-13T22:44:38.638317-06:00'
model: gpt-4-0125-preview
summary: "\u05E7\u05E4\u05E6\u05D5 \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05D0\u05DC\
  \ REPL \u05E9\u05DC \u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05E2\u05DC \u05D9\u05D3\
  \u05D9 \u05D4\u05E7\u05DC\u05D3\u05EA `python` \u05D1\u05E9\u05D5\u05E8\u05EA \u05D4\
  \u05E4\u05E7\u05D5\u05D3\u05D4 \u05E9\u05DC\u05DB\u05DD."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
weight: 34
---

## איך ל:
קפצו ישירות אל REPL של פייתון על ידי הקלדת `python` בשורת הפקודה שלכם. משם, בדקו פעולות פשוטות או קוד מרובה שורות:

```Python
>>> 1 + 1
2
>>> for i in range(3):
...     print(i)
...
0
1
2
```

נסו את עצמכם עם פונקציות וקבלת משוב מיידית:

```Python
>>> def greet(name):
...     return "Hello, " + name + "!"
...
>>> greet("Alice")
'Hello, Alice!'
```

שחקו עם ספריות וחקרו את התכונות שלהן בזמן אמת:

```Python
>>> import math
>>> math.sqrt(16)
4.0
```

יציאה באמצעות `exit()` מהירה או `Ctrl+D` (לפעמים `Ctrl+Z` בווינדוס).

## לעומק
המושג של REPL אינו ייחודי לפייתון; הוא כמו ישן כמו Lisp. רבות מהשפות מציעות את הסביבה האינטראקטיבית והמיידית הזו לגישה פרקטית לקוד. אלטרנטיבות למעטפת הפייתון המקורית כוללות את IPython ו-Jupyter Notebook, אשר מספקות אינטראקטיביות מוגברת, יותר תכונות, ואינטגרציה טובה יותר עם כלים אחרים. REPL הסטנדרטית של פייתון היא פשוטה, אבל היא מכילה את כל הכוח של פייתון, ומטפלת באובייקטים מורכבים ותוכניות רב-תהליכיות, אף על פי שהיא חסרה תכונות כמו השלמה אוטומטית והדגשת תחביר הנמצאות בכלים מתקדמים יותר.

## ראה גם
- [התיעוד הרשמי של פייתון על המפרש](https://docs.python.org/3/tutorial/interpreter.html)
- [IPython: מעטפת פייתון מתקדמת](https://ipython.org/)
- [פרויקט Jupyter](https://jupyter.org/)

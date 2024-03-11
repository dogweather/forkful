---
date: 2024-01-20 17:55:18.294916-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05D6\u05D4 \u05DC\u05E7\
  \u05D7\u05EA \u05D0\u05EA \u05D4\u05DE\u05D9\u05D3\u05E2 \u05E9\u05DE\u05D5\u05E8\
  \ \u05D1\u05E7\u05D5\u05D1\u05E5 \u05D5\u05DC\u05D4\u05E0\u05D2\u05D9\u05E9 \u05D0\
  \u05D5\u05EA\u05D5 \u05DC\u05E7\u05D5\u05D3 \u05E9\u05DC\u05E0\u05D5. \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D9 \u05D4\u05E8\u05D1\u05D4\
  \ \u05E4\u05E2\u05DE\u05D9\u05DD \u05DE\u05D9\u05D3\u05E2 \u05E9\u05E8\u05D5\u05E6\
  \u05D9\u05DD \u05DC\u05E2\u05D1\u05D3 \u05DE\u05D2\u05D9\u05E2 \u05D1\u05E6\u05D5\
  \u05E8\u05EA \u05E7\u05D1\u05E6\u05D9 \u05D8\u05E7\u05E1\u05D8."
lastmod: '2024-03-11T00:14:12.080839-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D1\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05D6\u05D4 \u05DC\u05E7\u05D7\
  \u05EA \u05D0\u05EA \u05D4\u05DE\u05D9\u05D3\u05E2 \u05E9\u05DE\u05D5\u05E8 \u05D1\
  \u05E7\u05D5\u05D1\u05E5 \u05D5\u05DC\u05D4\u05E0\u05D2\u05D9\u05E9 \u05D0\u05D5\
  \u05EA\u05D5 \u05DC\u05E7\u05D5\u05D3 \u05E9\u05DC\u05E0\u05D5. \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D9 \u05D4\u05E8\u05D1\u05D4 \u05E4\
  \u05E2\u05DE\u05D9\u05DD \u05DE\u05D9\u05D3\u05E2 \u05E9\u05E8\u05D5\u05E6\u05D9\
  \u05DD \u05DC\u05E2\u05D1\u05D3 \u05DE\u05D2\u05D9\u05E2 \u05D1\u05E6\u05D5\u05E8\
  \u05EA \u05E7\u05D1\u05E6\u05D9 \u05D8\u05E7\u05E1\u05D8."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת קובץ טקסט בפייתון זה לקחת את המידע שמור בקובץ ולהנגיש אותו לקוד שלנו. עושים את זה כי הרבה פעמים מידע שרוצים לעבד מגיע בצורת קבצי טקסט.

## איך לעשות:
קוד פשוט לקרוא מקובץ:

```Python
with open('example.txt', 'r', encoding='utf-8') as file:
    content = file.read()
    print(content)
```

פלט לדוגמה:
```
שלום, עולם!
דוגמה לטקסט בקובץ.
```

לקרוא כל שורה לתוך רשימה:

```Python
with open('example.txt', 'r', encoding='utf-8') as file:
    lines = file.readlines()
    print(lines)
```

פלט לדוגמה:
```
['שלום, עולם!\n', 'דוגמה לטקסט בקובץ.\n']
```

## עיון מעמיק
קריאת קבצים היא מרכיב בסיסי בתכנות מאז השנות ה-40. היא השתנתה במהלך השנים והיום בפייתון אפשר גם להשתמש ב`io` או `os` לשימושים מתקדמים. פייתון טיפל בבעיות של קידוד ומערכות הפעלה שונות כדי שאין צורך לדאוג להכל בעצמנו.

## תראו גם
- המסמך הרשמי למודול `open`: https://docs.python.org/3/library/functions.html#open
- המדריך למודול `io`: https://docs.python.org/3/library/io.html
- המדריך לטיפול בקבצים וחיתוכים: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files

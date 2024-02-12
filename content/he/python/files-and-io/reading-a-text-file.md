---
title:                "קריאת קובץ טקסט"
aliases: - /he/python/reading-a-text-file.md
date:                  2024-01-20T17:55:18.294916-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/reading-a-text-file.md"
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

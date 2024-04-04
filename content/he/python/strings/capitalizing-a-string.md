---
changelog:
- 2024-04-04 - dogweather - edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: #."
lastmod: '2024-04-04T00:27:07.183422-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 2
---

## איך לעשות:


### באמצעות המתודה המובנית של פייתון:
לפייתון יש מתודה מובנית `.capitalize()` עבור מחרוזות כדי לבצע פעולה זו בקלות.

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**פלט:**
```
Hello world
```

הנה ה`.capitalize()` המותאם אישית שלי שאני משתמש בו לבניית האתר הזה. הייתי צריך לוודא שמילים מיוחדות כמו **HTML** תמיד יישארו באותיות גדולות. זה גם מדגים [doctests](https://docs.python.org/3/library/doctest.html):

```python
def capitalize(string: str) -> str:
    """
    Capitalize a string, i.e. make the first letter uppercase.
    Handle special cases like "HTML".

    >>> capitalize("this is html, csv, xml, and http (no REPL).")
    'This is HTML, CSV, XML, and HTTP (no REPL).'

    >>> capitalize("this is json, VBA, an IDE, and yaml in the CLI.")
    'This is JSON, VBA, an IDE, and YAML in the CLI.'
    """
    return (
        string
            .capitalize()
            .replace('cli',  'CLI')
            .replace('csv',  'CSV')
            .replace('html', 'HTML')
            .replace('http', 'HTTP')
            .replace('ide',  'IDE')
            .replace('json', 'JSON')
            .replace('repl', 'REPL')
            .replace('vba',  'VBA')
            .replace('xml',  'XML')
            .replace('yaml', 'YAML')
    )

```




### טיפול במספר מילים:
לסיטואציות שבהן אתה רוצה כל מילה במחרוזת להתחיל עם אות גדולה (כמו כותרות), ניתן להחיל את המתודה `.title()`.

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**פלט:**
```
Python Programming Essentials
```

### שימוש בספריות צד שלישי:
למרות שספריית הסטנדרט של פייתון מצוידת לתיקון ראשי תיבות בסיסי, ספריות כמו `textblob` יכולות להציע בקרה מעודנת יותר, במיוחד לעיבוד שפה טבעית.

תחילה, וודא ש`textblob` מותקן אצלך:
```bash
pip install textblob
```

לאחר מכן, השתמש בו כדי לעשות ראשי תיבות למחרוזות, תוך זכירה שהראשי תיבות של `textblob` עשויים לעבוד אחרת בהתאם להקשר של השימוש:

```python
from textblob import TextBlob

my_sentence = "this is a test sentence"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**פלט:**
```
This is a test sentence
```

זכור, בזמן שהמתודות `capitalize()` ו`title()` שימושיות באופן כללי, שימוש בספריות כמו `textblob` יכול לספק גמישות נוספת ליישומים ספציפיים.

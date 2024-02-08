---
title:                "הגדלת אותיות במחרוזת"
aliases:
- he/python/capitalizing-a-string.md
date:                  2024-02-03T19:06:33.629381-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
לרשת מחרוזת פירושו להמיר את התו הראשון של מחרוזת לאות רישית ואת השאר לאותיות קטנות. פעולה זו נפוצה בעיבוד נתונים לנרמול קלטים או לשיפור הקריאות לכותרות, שמות וכדומה.


## איך לעשות:

### באמצעות שיטה מובנית של פייתון:
פייתון מציע שיטה מובנית `.capitalize()` למחרוזות על מנת לבצע משימה זו בקלות.

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**פלט:**
```
Hello world
```

### טיפול במספר מילים:
לתסריטים בהם רוצים שכל מילה במחרוזת תתחיל באות רישית (כמו בכותרות), ניתן להשתמש בשיטה `.title()`.

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
למרות שספריית הסטנדרט של פייתון מספקת כלים לרישום בסיסי של מחרוזות, ספריות כמו `textblob` יכולות להציע בקרה מעודנת יותר, במיוחד לעיבוד שפה טבעית.

ראשית, וודאו ש-`textblob` מותקן:
```bash
pip install textblob
```

לאחר מכן, השתמשו בו לרישום מחרוזות, תוך שימת לב שרישום של `textblob` עשוי לעבוד שונה בהתאם להקשר שבו נעשה בו שימוש:

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

זכרו, למרות שהשיטות `capitalize()` ו-`title()` שימושיות באופן כללי, הפעלת ספריות כמו `textblob` יכולה להציע גמישות נוספת ליישומים ספציפיים.

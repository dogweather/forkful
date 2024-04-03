---
date: 2024-01-20 17:48:30.706885-07:00
description: "How to: (\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA:) \u05D4\u05E9\
  \u05EA\u05DE\u05E9\u05D5 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D9\u05EA\
  \ `len()`."
lastmod: '2024-03-13T22:44:38.621655-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E9\u05EA\u05DE\u05E9\u05D5 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D9\u05EA `len()`."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
weight: 7
---

## How to: (איך לעשות:)
השתמשו בפונקציית `len()`:

```Python
my_string = "שלום עולם"
string_length = len(my_string)
print(string_length)  # תצפו לראות: 9
```

פלט:
```
9
```

## Deep Dive (צלילה עמוקה)
הפונקציה `len()` בפייתון היא בילט-אין מאז הגרסה הראשונה. יש אלטרנטיבות כמו לולאות, אבל הן לא יעילות כמו `len()`. מאחורי הקלעים, `len()` קוראת ל`__len__()` של המופע, זה כלול בהרבה אובייקטים מובנים כמו מחרוזות, רשימות, ועוד. אם אתם מממשים טיפוס מותאם אישית, ורוצים ש`len()` יעבוד עליו, תוסיפו מתודת `__len__()` לקלאס שלכם.

## See Also (ראו גם)
- התיעוד הרשמי של `len()`: https://docs.python.org/3/library/functions.html#len
- התיעוד של אובייקטים מותאמים אישית: https://docs.python.org/3/reference/datamodel.html#customizing-the-default-behavior

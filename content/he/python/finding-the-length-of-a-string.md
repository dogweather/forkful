---
title:                "מציאת אורך מחרוזת"
date:                  2024-01-20T17:48:30.706885-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
מציאת אורך מחרוזת היא פעולה שמחזירה את מספר התווים בה. פרוגרמרים עושים את זה כדי להתמודד עם קלט משתמש, לבדוק דרישות סיסמאות, לעבד טקסט ועוד.

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

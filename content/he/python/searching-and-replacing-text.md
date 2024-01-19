---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפת טקסט זה תהליך שבו מניפים את הטקסט על ידי איתור מונחים או פרקים מסוימים והחלפתם. תכנתים מבצעים את זה כדי לשנות נתונים, לשפר את תוכן הקוד או לתקן באגים.

## איך:
שימוש בפונקציות `replace()` ו`find()` של Python לחיפוש והחלפה טקסט.

```Python 
text = "היי, חברים!"
new_text = text.replace("היי", "שלום")
print(new_text)
```
הפלט:
```Python 
שלום, חברים!
```
שימוש בביטויים רגילים ב-Python:

```Python
import re
text = "אני מתכנת Python."
new_text = re.sub('Python', 'Java', text)
print(new_text)
```
הפלט:
```Python
אני מתכנת Java.
```
## צלילה עמוקה
חיפוש והחלפת טקסט הוא מרכזי לתכנות מאז התחלתה. חלק מהאפשרויות האחרות כוללות קסמים, אכן Python מציע דרך פשוטה ויעילה. Python מממש את זה באמצעות שימוש בתוכנת תרגול `str_replace()` או `sub()`.

## ראה גם
1. מדריך התחלה ‏‪Python‬‏: https://docs.python.org/he/3/tutorial/
2. מחברת Jupyter Python: https://jupyter-notebook.readthedocs.io/en/stable/
3. קוד מקור ‏‪Python‬‏: https://github.com/python/cpython
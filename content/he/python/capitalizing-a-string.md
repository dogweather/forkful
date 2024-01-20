---
title:                "הפיכת מחרוזת לאותיות רישיות"
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה "Capitalization" של מחרוזת? זהו שינוי של אותיות קטנות לאותיות גדולות. מתכנתים משתמשים בזה כדי להבליט כותרות, לעקוב אחר קונבנציית כתיבה, או לייחד מחרוזות מסוימות.

## How to:
הנה דוגמאות קוד ב-Python:

```python
# המרת האות הראשונה לגדולה
title = "היי, זו דוגמה לכותרת."
print(title.capitalize())
# Output: היי, זו דוגמה לכותרת.

# המרת כל המילים לאות גדולה בהתחלה
import string
print(string.capwords(title))
# Output: היי, זו דוגמה לכותרת.

# המרת כל מחרוזת לאותיות גדולות
shout = "צרחה בגדולות!"
print(shout.upper())
# Output: צרחה בגדולות!
```

## Deep Dive
בעבר, השימוש באותיות גדולות היה גם עניין של נראות במכונות כתיבה וספרות מודפסת. כיום, "Capitalization" בתכנות משמשת לעיצוב וכן להבדלה בין קוד מילולי ("Magic Strings") לבין משתנים או קבועים. לעיתים, קונבנציות כמו CamelCase או snake_case בשמות פונקציות ומשתנים מסייעות לתחזוקת קוד וקריאות.

ב-Python, ישנם כמה שיטות ל-capitalization של מחרוזות:
- `capitalize()` — ממירה את האות הראשונה לאות גדולה והשאר לקטנות.
- `title()` — כמו `capitalize()`, אך לכל מילים במחרוזת.
- `string.capwords()` — דומה ל-`title()`, אך מתעלמת מהגרש ("'") ודוגמאות נוספות.
- `upper()` — ממירה את כל האותיות במחרוזת לאותיות גדולות.

לכל שיטה יש את השימוש שלה, תלוי במטרת התוכנית.

## See Also
למידע נוסף, הנה כמה מקורות:
- [מדריך למתחילים בשפת Python](https://docs.python.org/3/tutorial/index.html)
- [התיעוד הרשמי של מחרוזות ב-Python](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [PEP 8 - הנחיות קוד אידיאלי ב-Python](https://peps.python.org/pep-0008/)
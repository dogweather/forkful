---
title:                "המרת מחרוזת לאותיות קטנות"
date:                  2024-01-20T17:39:08.230036-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה המרת מחרוזת לאותיות קטנות? זה שינוי של כל האותיות במחרוזת לאותיות קטנות. למה עושים את זה? כדי לייחד את התווים להשוואות, חיפושים ועוד מבלי להתחשב ברישיות.

## How to:
בפייתון, משתמשים במתודת `lower()` כדי להמיר כל התווים במחרוזת לקטנות.

```Python
original_string = "Python is FUN!"
lowercase_string = original_string.lower()
print(lowercase_string)
```

תוצאת ההדפסה תהיה:
```
python is fun!
```

זהו, פשוט כך.

## Deep Dive
לפני ימי Unicode, המרת אותיות לקטנות היתה פשוטה יותר. היום, עם Unicode, יש להתמודד עם תווים משפות רבות ומערכות כתיב שיכולות להיות מורכבות יותר. ב-Python, `str.lower()` מתמודדת עם זה כבר בשבילך.

אלטרנטיבות? יש כאלה. אם ברצונך להמיר לאותיות קטנות אך עם התחשבות במקרים מיוחדים של תווים (כמו בגרמנית), Python 3.3+ מציע את המתודה `casefold()`.

```Python
german_string = "Straße"
print(german_string.lower())
print(german_string.casefold())
```

תוצאה:
```
straße
strasse
```

מעוניינים ליצור את הורדת רמת האותיות בעצמכם? תזדקקו למיפוי של התווים ב-Unicode.

## See Also:
- [Python 3 Documentation for str.lower()](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Unicode Case Folding](https://www.unicode.org/reports/tr44/#CaseFolding)
- [Stack Overflow: How does Python lower() method work internally?](https://stackoverflow.com/questions/319426/how-does-python-lower-method-work-internally)
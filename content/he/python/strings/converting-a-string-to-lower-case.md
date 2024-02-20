---
date: 2024-01-20 17:39:08.230036-07:00
description: "\u05DE\u05D4 \u05D6\u05D4 \u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\
  \u05D5\u05EA? \u05D6\u05D4 \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E9\u05DC \u05DB\u05DC\
  \ \u05D4\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA\
  . \u05DC\u05DE\u05D4 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4? \u05DB\
  \u05D3\u05D9 \u05DC\u05D9\u05D9\u05D7\u05D3 \u05D0\u05EA \u05D4\u05EA\u05D5\u05D5\
  \u05D9\u05DD \u05DC\u05D4\u05E9\u05D5\u05D5\u05D0\u05D5\u05EA, \u05D7\u05D9\u05E4\
  \u05D5\u05E9\u05D9\u05DD \u05D5\u05E2\u05D5\u05D3 \u05DE\u05D1\u05DC\u05D9 \u05DC\
  \u05D4\u05EA\u05D7\u05E9\u05D1\u2026"
lastmod: 2024-02-19 22:04:57.880272
model: gpt-4-1106-preview
summary: "\u05DE\u05D4 \u05D6\u05D4 \u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\
  \u05EA? \u05D6\u05D4 \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E9\u05DC \u05DB\u05DC \u05D4\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA. \u05DC\
  \u05DE\u05D4 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4? \u05DB\u05D3\
  \u05D9 \u05DC\u05D9\u05D9\u05D7\u05D3 \u05D0\u05EA \u05D4\u05EA\u05D5\u05D5\u05D9\
  \u05DD \u05DC\u05D4\u05E9\u05D5\u05D5\u05D0\u05D5\u05EA, \u05D7\u05D9\u05E4\u05D5\
  \u05E9\u05D9\u05DD \u05D5\u05E2\u05D5\u05D3 \u05DE\u05D1\u05DC\u05D9 \u05DC\u05D4\
  \u05EA\u05D7\u05E9\u05D1\u2026"
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
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

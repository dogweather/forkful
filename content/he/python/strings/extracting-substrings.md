---
date: 2024-01-20 17:46:49.383998-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DE\u05D0\u05D6\
  \ \u05D4\u05E7\u05D3\u05DE\u05D5\u05DF \u05E9\u05DC \u05E4\u05D9\u05D9\u05EA\u05D5\
  \u05DF, \u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05D4\u05D9\u05D4 \u05D7\u05DC\u05E7 \u05DE\u05D4\u05E9\u05E4\u05D4\
  . \u05D0\u05D9\u05E0\u05D3\u05E7\u05E1\u05D9\u05DD \u05D1\u05E4\u05D9\u05D9\u05EA\
  \u05D5\u05DF \u05DE\u05EA\u05D7\u05D9\u05DC\u05D9\u05DD \u05DE-0. \u05D4\u05D0\u05D9\
  \u05E0\u05D3\u05E7\u05E1 \u05D4\u05E9\u05E0\u05D9 \u05D1\u05D8\u05D5\u05D5\u05D7\
  \ \u05E0\u05E4\u05E1\u05DC \u05DE\u05D4\u05D7\u05D9\u05DC\u05D5\u05E5. \u05D2\u05DD\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 slice() \u05E2\u05D5\u05D1\u05D3\u05EA\
  \u2026"
lastmod: '2024-04-05T21:53:39.947555-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D0\u05D6 \u05D4\u05E7\u05D3\u05DE\u05D5\u05DF \u05E9\u05DC \u05E4\
  \u05D9\u05D9\u05EA\u05D5\u05DF, \u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D4\u05D9\u05D4 \u05D7\u05DC\u05E7 \u05DE\
  \u05D4\u05E9\u05E4\u05D4."
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
weight: 6
---

## איך לעשות:
```Python
# דוגמה לחילוץ תת-מחרוזת בפייתון

# מחרוזת דוגמה
text = "שלום, עולם!"

# חילוץ תת-מחרוזת על ידי טווח אינדקסים
substring = text[7:12]
print(substring)  # עולם

# חילוץ תת-מחרוזת באמצעות שיטת slice()
slice_substring = text[slice(7, 12)]
print(slice_substring)  # עולם

# חילוץ תת-מחרוזת מהתחלה עד אינדקס ספציפי
start_substring = text[:5]
print(start_substring)  # שלום,

# חילוץ תת-מחרוזת מאינדקס ספציפי ועד הסוף
end_substring = text[6:]
print(end_substring)  # עולם!
```

## עומק המידע
מאז הקדמון של פייתון, חילוץ תת-מחרוזות היה חלק מהשפה. אינדקסים בפייתון מתחילים מ-0. האינדקס השני בטווח נפסל מהחילוץ. גם פונקציה slice() עובדת עם אינדקסים בדומה. אם שכחת את האינדקס הראשון, היא תתחיל מההתחלה. אם שכחת את האינדקס השני, היא תלך עד הסוף. יש גם רשימות ומחרוזות וגם מאפיינים מעניינים אחרים, כולל חלוקה לתת-מחרוזות לפי מחרוזת חותך עם split(), וחיפוש תת-מחרוזת עם find() או index().

## לראות גם
- דוקומנטציה של פייתון על מחרוזות: https://docs.python.org/3/library/stdtypes.html#string-methods
- מדריך פייתון לחילוץ תת-מחרוזות: https://www.programiz.com/python-programming/methods/string
- W3Schools - מחרוזות בפייתון: https://www.w3schools.com/python/python_strings.asp

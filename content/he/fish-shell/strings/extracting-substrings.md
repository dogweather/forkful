---
date: 2024-01-20 17:46:27.616728-07:00
description: "\u05DE\u05D4 \u05D6\u05D4 \u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA\
  -\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D5\u05DC\u05DE\u05D4 \u05D6\u05D4\
  \ \u05D7\u05E9\u05D5\u05D1? \u05D1\u05E7\u05E6\u05E8\u05D4, \u05D6\u05D4 \u05E4\u05E2\
  \u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D0\u05E0\u05D5 \u05DC\u05D5\u05E7\u05D7\
  \u05D9\u05DD \u05D7\u05DC\u05E7 \u05DE\u05EA\u05D5\u05DB\u05DF \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05E7\u05D9\u05D9\u05DE\u05EA. \u05EA\u05DB\u05E0\u05D5\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\
  \u05D9 \u05DC\u05E2\u05D1\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05E9\
  \u05DC\u05D5\u05E3 \u05DE\u05D9\u05D3\u05E2 \u05E1\u05E4\u05E6\u05D9\u05E4\u05D9\
  , \u05D0\u05D5\u2026"
lastmod: '2024-03-13T22:44:40.026730-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D4 \u05D6\u05D4 \u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D5\u05DC\u05DE\u05D4 \u05D6\u05D4 \u05D7\
  \u05E9\u05D5\u05D1."
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
weight: 6
---

## How to:
ב-Fish Shell, חילוץ תת-מחרוזות הוא פשוט. נבדוק כמה דוגמאות:

```Fish Shell
# דוגמה 1: חילוץ תת-מחרוזת מתוך מחרוזת.
set -l my_string "Hello, world!"
echo $my_string[1..5]  # יוצא Hello

# דוגמה 2: חילוץ מתחילת המחרוזת עד תו מסוים.
echo $my_string[..5]  # יוצא Hello

# דוגמה 3: חילוץ מתו מסוים עד סוף המחרוזת.
echo $my_string[8..-1]  # יוצא world!

# דוגמה 4: חילוץ תת-מחרוזת באמצעות נקודות קץ שליליות.
echo $my_string[-6..-2]  # יוצא world
```

ניתן לשים לב כי הדפסנו חלקים שונים של המחרוזת על ידי ציון המקום התחלתי והסופי בסוגריים מרובעים.

## Deep Dive
ב-Fish Shell, חילוץ מחרוזות אינו מסובך, אך יש כמה פרטים לזכור:
1. האינדקסים מתחילים מ-1, לא מ-0 כמו בשפות אחרות.
2. ניתן להשתמש באינדקסים שליליים לחילוץ מהסוף להתחלה.
3. בשפות תכנות אחרות, נמצאים פקודות חילוץ מחרוזות אחרות, כמו `.substring()` ב-Java או `substr()` ב-PHP. במקרים אלו, ישנן כללים שונים וסינטקס משתנה.
4. בהיסטוריה, מערכות שונות ושפות תכנות פיתחו מגוון דרכים לעשות פעולת חילוץ, אך המטרה תמיד הייתה זהה - לאפשר גישה ושינוי לחלקים מסוימים בתוך מחרוזת.

## See Also
1. הדוקומנטציה הרשמית של Fish Shell - [Substring Expansion](https://fishshell.com/docs/current/index.html#expand-index-range)
2. מדריך לשפות תכנות אחרות על חילוץ מחרוזות - [W3Schools: JavaScript String slice()](https://www.w3schools.com/jsref/jsref_slice_string.asp)
3. פורום עזרה לשאלות על Fish Shell - [Stack Overflow](https://stackoverflow.com/questions/tagged/fish)

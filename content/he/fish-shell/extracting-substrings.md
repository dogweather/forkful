---
title:                "חילוץ תת-מחרוזות"
date:                  2024-01-20T17:46:27.616728-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"

category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה חילוץ תת-מחרוזות ולמה זה חשוב? בקצרה, זה פעולה שבה אנו לוקחים חלק מתוכן מחרוזת קיימת. תכנותים עושים את זה כדי לעבד נתונים, לשלוף מידע ספציפי, או לשנות פורמטים.

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

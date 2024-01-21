---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:48:39.225331-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
גנרטורים של מספרים אקראיים ב-Bash נותנים לך פלט שאי אפשר לנחש אותו מראש. מתכנתים משתמשים בזה לבדיקות, אבטחה, גיימינג, ועוד.

## How to: (איך לעשות:)
גנרטור בסיסי: תוצאה בין 1 ל-100
```Bash
echo $((RANDOM % 100 + 1))
```
גנרטור מתקדם: עם `shuf`
```Bash
shuf -i 1-100 -n 1
```
תוצאות לדוגמא:
```
57
84
```

## Deep Dive (צלילה עמוקה)
המספר `$RANDOM` הופיע לראשונה ב ksh (Korn shell) בשנות ה-80. אלטרנטיבות כוללות את `shuf`, וכן שימוש ב `/dev/urandom` לאבטחה גבוהה יותר. ביצוע הקוד מתבצע בזמן ריצה ותלוי בתהליכים פנימיים של המערכת הפועלת, ולכן אף פעם לא יהיה זהה.

## See Also (ראה גם)
- [Random Numbers in Bash](https://tldp.org/LDP/abs/html/randomvar.html) - מדריך מעמיק על מספרים אקראיים ב-Bash.
- [Using shuf and seq](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html) - דוקומנטציה לכלי `shuf`.
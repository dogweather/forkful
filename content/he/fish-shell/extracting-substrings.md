---
title:                "החלצת תת מחרוזות"
html_title:           "Fish Shell: החלצת תת מחרוזות"
simple_title:         "החלצת תת מחרוזות"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

הכוונה ב"לחלץ מחרוזות" היא לקבל חתיכה מתוך מחרוזת גדולה יותר. פעולה זו שימושית כאשר נרצה לעבוד עם חלק מסוים של מידע מתוך מחרוזת גדולה יותר. מתכנתים משתמשים בפעולה זו כדי להפריד חלקים שונים של טקסט או מידע ולעבוד איתם בנפרד.

## How to:

 כדי לחלץ מחרוזת ב-Fish Shell, ניתן להשתמש בפקודה `string sub` עם הפרמטרים הבאים: מה אנחנו רוצים לחלץ, המחרוזת שבו אנחנו רוצים לחלץ ממנו והאינדקסים של התווים שאליהם אנחנו רוצים לחלץ. לדוגמא:

```
Fish Shell string sub "hello" h 0; #Output: "hello"
```

ניתן גם להשתמש בפקודת `string match` כדי לחלץ מחרוזת תת-מחרוזת לפי תבנית מסוימת. לדוגמא:

```
Fish Shell string match "hello world" "hello *"; #Output: "hello"
```

## Deep Dive:

פעולת המחרוזת ב-Fish Shell ביסודה מבוססת על פקודת `substr` שניתן למצוא בכלי Unix של POSIX. פקודה זו אינה מתאימה לכל המצבים ולכן נוצרה פקודת `string sub` ב-Fish Shell כדי להתאים לשפה בצורה טובה יותר.

ישנן פתרונות נוספים לחילוץ מחרוזות כגון שימוש בפקודת `grep` או בכלי תוכנתיים כמו `awk` ו־`sed`. פקודת `string sub` היא יעילה יותר עבור מצבים מסוימים שבהם אנחנו רוצים לחלץ תת-מחרוזת בדיוק לפי האינדקסים של התווים.

## See Also:

למידע נוסף על פקודת `string sub` ניתן לקרוא בדף הרשמי של Fish Shell:
https://fishshell.com/docs/current/cmds/string-sub.html
---
title:                "שרשור מחרוזות"
date:                  2024-01-20T17:34:28.130973-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרשור מחרוזות"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
צירוף מחרוזות ב-Bash זו צורה לחבר טקסטים למחרוזת אחת. מתכנתים עושים זאת כדי להכין פלט מותאם אישית או לבנות פקודות ונתיבי קבצים דינמית.

## How to (איך לעשות?)
ב-Bash אתה יכול לצרף מחרוזות פשוט על ידי שימוש בציטוטים או ללא הם. הנה כמה דוגמאות:

```Bash
# צירוף עם מרווח
greeting="שלום"
name="עולם"
welcome_message="$greeting, $name!"
echo $welcome_message
```

פלט:
```
שלום, עולם!
```

```Bash
# צירוף בלי מרווח
prefix="מתכנת"
suffix="מאושר"
status="$prefix$suffix"
echo $status
```

פלט:
```
מתכנתמאושר
```

## Deep Dive (צלילה עמוקה)
במערכות Unix ו-Linux המוקדמות, צירוף מחרוזות הוא פעולה בסיסית שמשמשת תחזוקה של סקריפטים ועבודה עם טקסט. ב-Bash, הפעולה הזו פשוטה אך חזקה. אלטרנטיבות כוללות שימוש בפקודות כמו `printf` או הפעולה `concat` (במקרים מסוימים). קריאה לזיכרון: מחרוזת ב-Bash היא פרימיטיב שאינו דורש הגדרה מפורשת כמו בשפות אחרות – תופס זה מיוחס אוטומטית.

## See Also (ראו גם)
2. [Advanced Bash-Scripting Guide: String Operations](https://tldp.org/LDP/abs/html/string-manipulation.html)
3. [Bash Guide for Beginners: Chapter 3. Working with Strings](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_03_03.html)

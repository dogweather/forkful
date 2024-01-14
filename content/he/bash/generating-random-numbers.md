---
title:                "Bash: יְּצִירָת מספרים אֲקָרִים"
programming_language: "Bash"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

# למה

למה לכתוב פקודות הגרלה בבאש? כי פקודות הגרלה משמשות בתוכניות רבות כדי ליצור מספרים אקראיים שיכולים לשמש כמפתחות אבטחה, ליצירת נתונים אקראיים לצורך ניסויים ועוד.

# כיצד לעשות את זה

```bash
# פקודת הגרלה פשוטה בבאש היא $RANDOM. יש להשתמש בערך המוחזר על-ידי הפקודה ליצירת מספרים אקראיים.
echo $RANDOM  # יוצא: 21471
```

ישנן גם אפשרויות נוספות להגרלת מספרים בבאש, כמו להשתמש בפקודת `shuf` ובמגוון רחב של אפשרויות להגדיר טווחי מספרים ומספר הגרלות.

```bash
# פקודת shuf תגריל מספרים בין 1 ל-50
shuf -i 1-50 -n 1  # יוצא: 24
```

# צלילת עומק

פקודות הגרלה משתמשות באלגוריתמים מתימטיים כדי ליצור מספרים אקראיים. אלו כוללים הגרלת מספרים ע"י טווחי מספרים והגרלת מספרים מתוך ליתות.

# ראה גם

- [מדריך לפקודת הגרלה של באש](https://www.cyberciti.biz/faq/bash-random-number/)
- [מדריך מתקדם לפקודת shuf](https://www.linuxcommand.org/man_pages/shuf1.html)
- [אלגוריתם מספרים אקראיים](https://www.geeksforgeeks.org/random-number-generator-in-arithmetic/)
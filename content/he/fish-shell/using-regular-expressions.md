---
title:                "שימוש בביטויים רגולריים"
date:                  2024-01-19
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
רגולר אקספרשנז (ביטויים רגולריים) זה כלי לחיפוש והחלפת טקסט לפי תבנית. מתכנתים משתמשים בזה כדי לעבוד עם טקסטים בצורה יעילה ומהירה.

## איך לעשות:
```Fish Shell
echo "האם יש מישהו פה?" | grep -oP "מישהו"
# פלט: מישהו

echo "דגל התכנות שלי: --regex=^מישהו" | string replace -r -- "מישהו" "אף אחד"
# פלט: דגל התכנות שלי: --regex=^אף אחד
```

## צלילה עמוקה
רגולר אקספרשנז התפתחו בשנות ה-50 וה-60 על ידי ווארן טאג וקנ תומפסון. יש אלטרנטיבות כמו globbing שנמצאות בשלל שפות סקריפטינג. Fish משתמש בסינטקס POSIX ERE (Extended Regular Expressions) עבור רגולר אקספרשנז.

## ראו גם
- הדוקומנטציה הרשמית של Fish בנושא ביטויים רגולריים: [Fish Shell Regular Expressions](https://fishshell.com/docs/current/index.html#syntax-regular-expressions)
- תיעוד גרפ (grep): [GNU Grep](https://www.gnu.org/software/grep/manual/grep.html)
- תיעוד אובוינטו על string ב-Fish Shell: [Fish Shell String](https://fishshell.com/docs/current/cmds/string.html)

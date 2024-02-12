---
title:                "התחלת פרויקט חדש"
aliases:
- /he/bash/starting-a-new-project.md
date:                  2024-01-20T18:03:15.716411-07:00
model:                 gpt-4-1106-preview
simple_title:         "התחלת פרויקט חדש"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה פרויקט חדש ולמה אנחנו מתחילים אותו? זה מתחיל ברעיון שמגובש לקוד. תוכניתנים יוצרים פרויקטים חדשים כדי לפתור בעיות, ללמוד, ולבנות משהו חדש.

## How to:
כדי להתחיל פרויקט חדש ב-Bash, לרוב נרצה ליצור סקריפט בסיסי:

```Bash
#!/bin/bash
# קובץ my_script.sh
echo "שלום, עולם!"
```

אחרי ששמרנו את הקובץ, נעשה אותו ניתן להרצה:
```Bash
chmod +x my_script.sh
```

ונריץ אותו:
```Bash
./my_script.sh
```

הפלט יהיה:
```
שלום, עולם!
```

## Deep Dive
ל-Bash יש מקום מיוחד בלב הניקס. הוא נוצר ב-1989, ומאז נחשב לכלי שורת הפקודה הסטנדרטי ברוב הגרסאות של לינוקס ו-UNIX. ישנם אלטרנטיבות כמו Zsh או Fish, שמציעות יכולות חדשות. עם זאת, Bash הוא הפופולרי ביותר משום שהוא קיים כמעט בכל מקום ותומך בסקריפטים מהפקודות של המערכת.

## See Also
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/)
- [ShellCheck – tool for detecting issues in bash scripts](https://www.shellcheck.net/)

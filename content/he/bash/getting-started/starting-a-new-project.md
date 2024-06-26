---
date: 2024-01-20 18:03:15.716411-07:00
description: "How to: \u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC \u05E4\
  \u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\u05D3\u05E9 \u05D1-Bash, \u05DC\u05E8\u05D5\
  \u05D1 \u05E0\u05E8\u05E6\u05D4 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05E1\u05E7\u05E8\
  \u05D9\u05E4\u05D8 \u05D1\u05E1\u05D9\u05E1\u05D9."
lastmod: '2024-03-13T22:44:39.625802-06:00'
model: gpt-4-1106-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC \u05E4\u05E8\u05D5\
  \u05D9\u05E7\u05D8 \u05D7\u05D3\u05E9 \u05D1-Bash, \u05DC\u05E8\u05D5\u05D1 \u05E0\
  \u05E8\u05E6\u05D4 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05E1\u05E7\u05E8\u05D9\u05E4\
  \u05D8 \u05D1\u05E1\u05D9\u05E1\u05D9."
title: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9"
weight: 1
---

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

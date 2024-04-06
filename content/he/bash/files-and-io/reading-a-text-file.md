---
date: 2024-01-20 17:53:56.227675-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E7\u05D5\u05D3\
  \ \u05D5\u05D3\u05D5\u05D2\u05DE\u05D0\u05D5\u05EA \u05E4\u05DC\u05D8."
lastmod: '2024-04-05T21:53:40.765590-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 22
---

## איך לעשות:
קוד ודוגמאות פלט.

```Bash
# קריאת קובץ שורה אחר שורה
while IFS= read -r line; do
  echo "שורה: $line"
done < "example.txt"
```
פלט:
```
שורה: שורה ראשונה בקובץ
שורה: שורה שנייה בקובץ
...
```

```Bash
# לקרוא קובץ כולו לתוך משתנה
file_content=$(< "example.txt")
echo "$file_content"
```
פלט:

```
שורה ראשונה בקובץ
שורה שנייה בקובץ
...
```

## צלילה לעומק:
בעבר, תוכניות וסקריפטים נקראו מקלטים סרטים וטייפים, כאשר מערכות הפעלה מודרניות הפכו את קריאת קבצים לפעולה פשוטה ומהירה. ב-Bash, ניתן לקרוא קבצים בדרכים שונות, מהפקודה `cat` ועד לימוד שורה בשורה עם לולאת `while`. שימוש ב-IFS (Internal Field Separator) מאפשר שליטה על אופן פיצול הנתונים בזמן קריאה. גודל הקובץ, אופי התוכן ודרישות הביצוע משפיעים על השיטה המועדפת לקריאה.

## ראו גם:
- [Bash scripting tutorial](https://www.shellscript.sh)
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/)
- [GNU Bash documentation](https://www.gnu.org/software/bash/manual/)

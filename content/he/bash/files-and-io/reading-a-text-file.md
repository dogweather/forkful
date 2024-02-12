---
title:                "קריאת קובץ טקסט"
date:                  2024-01-20T17:53:56.227675-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת קובץ טקסט ב-Bash היא פעולה בסיסית שמאפשרת לנו לטעון תוכן לתוך משתנים ולעבוד איתו. מתכנתים קוראים קבצי טקסט כדי לנתח נתונים, להפעיל סקריפטים אוטומטיים ולבצע תחזוקה של מערכות.

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

---
title:                "שימוש בביטויים רגולריים"
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
רגולר אקספרשנס (ביטויים רגולריים) זה שם מתוחכם למשהו פשוט: כללים לחיפוש והחלפת טקסט לפי תבנית. תכנתנים משתמשים בזה בשביל לעבד טקסט בצורה יעילה וחכמה, בין אם זה למצוא מידע, לאמת מבנים, או לנקות נתונים.

## איך עושים את זה:
```Bash
# חיפוש טקסט שמתחיל ב-abc
echo "abcdef" | grep "^abc"

# תוצאה
abcdef

# חיפוש מספרים בטקסט
echo "my number is 1234" | grep -o "[0-9]+"

# תוצאה
1234

# החלפת טקסט מתוך קובץ
sed -i 's/old-text/new-text/g' example.txt
```

## טבילה עמוקה
ביטויים רגולריים הם לא חידוש – החלו ככללי חיפוש בעיבוד טקסט מהזמנים ורשת היחידות. ישנם אלטרנטיבות כמו פונקציות חיפוש קבועות או שפות תכנות עם פקד טקסט מובנה. באש בוצע עיבוד רגולר אקספרשנס דרך כלים כמו grep, sed, וawk.

## ראו גם:
- [רשמית גנו grep](https://www.gnu.org/software/grep/manual/grep.html)
- [סדנאות Regex למתחילים](https://regexone.com/)
- [תיעוד sed בלינוקס](https://www.gnu.org/software/sed/manual/sed.html)

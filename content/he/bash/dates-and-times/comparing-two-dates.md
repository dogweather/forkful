---
date: 2024-01-20 17:32:55.735064-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DC\u05D4\u05DC\
  \u05DF \u05D3\u05D5\u05D2\u05DE\u05D0\u05D5\u05EA \u05DC\u05E9\u05D9\u05DE\u05D5\
  \u05E9 \u05D1\u05EA\u05E1\u05E8\u05D9\u05D8\u05D9 Bash \u05DC\u05D4\u05E9\u05D5\u05D5\
  \u05D0\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD."
lastmod: '2024-03-13T22:44:39.645600-06:00'
model: gpt-4-1106-preview
summary: "\u05DC\u05D4\u05DC\u05DF \u05D3\u05D5\u05D2\u05DE\u05D0\u05D5\u05EA \u05DC\
  \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05EA\u05E1\u05E8\u05D9\u05D8\u05D9 Bash \u05DC\
  \u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
weight: 27
---

## איך לעשות:
להלן דוגמאות לשימוש בתסריטי Bash להשוואת תאריכים:

```Bash
# הגדרת שני תאריכים בפורמט YYYY-MM-DD
date1="2023-02-10"
date2="2023-02-15"

# המרת התאריכים לפרמט תאריך שניתן להשוואה
sec1=$(date -d "$date1" +%s)
sec2=$(date -d "$date2" +%s)

# בדיקת מי מהם קודם לכן והדפסת התוצאה
if [ $sec1 -lt $sec2 ]; then
    echo "$date1 is earlier than $date2"
elif [ $sec1 -gt $sec2 ]; then
    echo "$date2 is earlier than $date1"
else
    echo "Both dates are the same"
fi
```

פלט לדוגמה:
```
2023-02-10 is earlier than 2023-02-15
```

## עיון מעמיק:
השוואת תאריכים ב-Bash מתבצעת על ידי המרת התאריכים לשניות מאז התאריך המסוימת (epoch time) ולאחר מכן השוואת המספרים. תאריכים במערכות הפעלה מודרניות מתחילים להיספר מהתאריך 1 בינואר 1970 (הנקרא Unix epoch). קיימים תסריטים חלופיים, כמו למשל פקודת `[[ $date1 < $date2 ]]`, אבל הם פחות נפוצים ולא תמיד מדויקים בגלל פורמטים שונים. אין טיפול ישיר ב-Bash באזורים זמניים או שמירת שעון קיץ, לכן צריך להקפיד להשוואה באזור זמן קבוע או לבצע המרות מתאימות.

## ראה גם:
- [Bash Date Command](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html) - המדריך הרשמי לפקודת `date`
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/) - מדריך מתקדם לכתיבת תסריטי Bash
- [Unix Time](https://en.wikipedia.org/wiki/Unix_time) - הסבר על Unix epoch time בוויקיפדיה

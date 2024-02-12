---
title:                "השוואת שתי תאריכים"
aliases:
- /he/bash/comparing-two-dates.md
date:                  2024-01-20T17:32:55.735064-07:00
model:                 gpt-4-1106-preview
simple_title:         "השוואת שתי תאריכים"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
השוואת שתי תאריכים ב-Bash היא פעולה שבודקת את ההפרש בין תאריכים לצרכי לוגיקה מתכנתית או סדר זמני. מתכנתים עושים זאת לסינכרון תהליכים, תזמון אירועים או בדיקת התקפות תוקפים.

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

---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

שדרוג תאריך ממחרוזת מדובר על פעולה בה אנו מבצעים את המחרוזת, לרוב משתמש, ומפענחים את ערכי היום, החודש והשנה. תיכופי במיוחד, אנו נזקקים עבור זה עריריה אפשרויות הזנה של המשתמשים ואת טיפול הנתונים.

## איך:

באם גרסת ה-Bash שלך היא 4.2 או חדשה, אתה יכול להשתמש ב- `printf` בשילוב עם פונקצית התאריך. 

```Bash
date_unformatted='2022-5-1'
formatted_date=$(date -d"$date_unformatted" +'%d/%m/%Y')
printf "%s\n" "$formatted_date"
```

הפלט המיוצר יהיה:

```Bash
01/05/2022
```

## צלילה עמוקה:

השדרוג של נתונים הוא מישור מרכזי בראות יישומי המחשב שלנו. מאז שמחשבים התחילו לנהל תאריכים ושעות, בעיות תאריך רבות נפתרו. ישנן גם חלופות כמו `dateutils` אשר הן ספריות המתממשקות לטיפול משובח בתאריכים. עם `date` ב-Bash, עלינו לנקוט לב שהפורמט הנתמך הוא 'YYYY-MM-DD'. כשאנחנו מפענחים תאריך, אנו מניחים כי המחרוזת המיובאת עומדת באותו פורמט. 

## ראו גם:

חפש אחר מאמרים נוספים בנושאים קרובים:
- [Linux date command (nixCraft)](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)
- [How to Parse a Date From a String (StackOverflow)](https://stackoverflow.com/questions/10286204/the-right-json-date-format)
- [The GNU `date` manual](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:14:24.774415-07:00
simple_title:         "קבלת התאריך הנוכחי"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
שגריר התאריך הנוכחי בפיש של זה פשוט – זה הצגת הזמן והתאריך של "עכשיו". תכנתים צריכים את זה ליומנים, טיימרים, וכשהם מייצרים קבצים עם חותמות זמן.

## איך לעשות:
קוד פיש להשגת התאריך הנוכחי:

```Fish Shell
set -l current_date (date)
echo $current_date
```

דוגמת פלט:
```
Thu Mar 4 12:05:36 PST 2023
```
להחלפת פורמט תאריך:

```Fish Shell
set -l formatted_date (date "+%Y-%m-%d %H:%M:%S")
echo $formatted_date
```

דוגמת פלט:
```
2023-03-04 12:05:36
```

## צלילה עמוקה:
בעולם Unix, הפקודה `date` משמשת מאז שנות ה-70 להשגת מידע על הזמן והתאריך. בפיש, דומה לשל קלאסיות אחרות, `date` מופעל ישירות מהשורת הפקודה עם אפשרויות לפורמטינג חופשי. בעוד שיש אלטרנטיבות כמו פיתון או פי.אייץ'פי המאפשרים פעולות מתקדמות יותר עם תאריכים, הפקודה `date` בשל נותרת מהירה ונפוצה לשימושים פשוטים וכשמהירות היא קריטית.

## ראה גם:
- [פיש של דוקומנטציה – עבודה עם תאריכים](https://fishshell.com/docs/current/index.html#dates)
- [תיעוד פקודת Date של GNU](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [מדריך לפורמטינג תאריכים ב-Unix](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)

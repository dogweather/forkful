---
title:                "עבודה עם קובץ CSV"
html_title:           "Fish Shell: עבודה עם קובץ CSV"
simple_title:         "עבודה עם קובץ CSV"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם קבצי CSV היא תהליך נפוץ בקרב מתכנתים. זהו פורמט נתונים נפוץ שמאפשר לנו לשמור ולטעון מידע מורכב כמו טבלאות ונתוני רשומות בצורה ברורה ומאוחרת. מתכנתים משתמשים בקבצי CSV כדי לסדר ולטעון נתונים בפרויקטים שונים, כמו אפליקציות ואתרים.

## איך לעבוד עם קבצי CSV במכסת של Fish?


```
set -g IFS ','

set rows (cat test.csv | sed 's/"//g')

for row in $rows
    set values (echo $row | sed 's/ /,/g')
    echo "First column: $values[1]"
    echo "Second column: $values[2]"
end
```

כאן, אנו משתמשים בסקריפט קל כדי לקרוא ולעבוד עם קובץ CSV במכסת של Fish. נריץ פקודות שמסייעות לנו לטעון ולקרוא נתוני CSV.

## מידה עמוק

קבצי CSV היו קיימים כבר מזמן רב, והם נחשבים לפורמט נתונים יעיל ונכון עבור נתוני גיבוב כמו נתונים סטטיסטיים ומידע גרפי. ישנן מספר אלטרנטיבות גיבוב שונות, אך קבצי CSV נחשבים לתקן שהידע נפוץ ותומך בהם. כפי שניתן לראות בדוגמאות, עבודה עם קבצי CSV במכסת של Fish היא פשוטה וידידותית.

## ראו גם

- [דף התיעוד הרשמי של פקודת Fish shell לעבוד עם CSV](https://fishshell.com/docs/current/cmds/set.html)
- [פקודת set של Linux](https://man7.org/linux/man-pages/man1/set.1p.html)
---
title:                "Fish Shell: עובדים עם json"
simple_title:         "עובדים עם json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## למה
נחשב לעדיף תקשורתית ומקצועית, JSON הוא לוצ'ר טכנולוגיה נפוצה במערכות מידע ותכניתיות. השתמש ב- JSON כדי לקלוט, לאחזר ולערוך נתונים בפורמט פשוט וקריא. בכתיבת תוכניות מחשב עם Fish Shell, הפרמטרים נמצאים בקלות, JSON בנוי נכון במעמד כלי עבודה.

## איך ל
קלוט נתונים מקובץ JSON לתוך משתנה ב-Fish Shell:

```
set -l data (cat file.json | from-json)
echo $data[0]
```

כאן, אנו משתמשים בכלי העבודה `from-json` כדי לקלוט נתונים מתוך קובץ נתונים בפורמט JSON ולשמור אותם במשתנה בשם `data`. לאחר מכן, אנו מדפיסים את אלמנט הראשון במערך ה- `data`. להלן פלט הקלט והפלט:

קלט (file.json):
```
[
    {
        "name": "Neta",
        "age": 25,
        "occupation": "Developer"
    },
    {
        "name": "Tom",
        "age": 30,
        "occupation": "Designer"
    }
]
```

פלט:
```
Neta
```

## צלילה עמוקה
היכן שיש JSON, יש גם דרכים נוחות יותר לעבוד עם נתונים ב-Fish Shell. הנה כמה קודקודים נוספים כדי לעזור לך להתחיל:

- [התיעדות של Fish Shell על JSON](https://fishshell.com/docs/current/cmds/from-json.html)
- [כלי חיפוש והחלפה מהירים לקבצי JSON](https://github.com/fishpkg/fish-json)
- [תיאור מפורט ותיעוד של פרמטרים שונים שנתמכים עם Fish Shell JSON כלומר מחברי](https://fishshell.com/docs/current/cmds/from-json.html)
- [מחברת Blog על השתמשות כיוון JSON בכתיבת תוכניות עם Fish Shell](https://dmitryfrank.com/articles/shell_programming_with_json)

## ראו גם
- פרויקט [Fish Shell](https://fishshell.com/)
- [מדריך עבור מתחילים בבניית פיס תוכניות עם כוונת JSON](https://www.linuxquestions.org/questions/programming-9/json-with-awk-and-bash-855102/)
- [מדריך מפורט על 7 צעדים בכדי
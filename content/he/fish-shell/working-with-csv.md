---
title:                "עבודה עם קבצי CSV"
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם קבצי CSV משמשת לטיפול בנתונים טבולריים - שמורים כטקסט עם פירודי פסיקים. תוכניתנים משתמשים בזה כי זה פורמט פשוט ונפוץ לייבא ולייצא נתונים בין מערכות ותוכנות שונות.

## איך לעשות:
```Fish Shell
# קריאת CSV והדפסת השורה הראשונה
begin; set -l IFS ,; cat example.csv | read -la column_headers; echo $column_headers; end

# חיתוך עמודה מסוימת מכל השורות
cut -d ',' -f 2 example.csv

# סינון והדפסת שורות שמכילות "מילת מפתח"
grep 'מילת מפתח' example.csv
```
תוצאות דוגמא:
```
שדה1,שדה2,שדה3
ערך1,ערך2,ערך3
```

## צלילה עמוקה
CSV נוצר בשנות ה-70 ומייצג פשטות וסטנדרטיזציה בייצוג נתונים. ישנם פורמטים חלופיים כמו JSON או XML אבל הם לעיתים יותר מורכבים לקריאה וכתיבה ידנית. Fish Shell תומך ביביאת נתונים מCSV באמצעות פקודות Unix קלאסיות ואופרטורים כמו pipe (`|`).

## ראה גם
- מדריך ל- Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- תרגול עם נתוני CSV ב- Fish: [https://github.com/fish-shell/fish-shell/wiki/Tutorial#using-commands](https://github.com/fish-shell/fish-shell/wiki/Tutorial#using-commands)
- הסבר על פורמט CSV: [https://en.wikipedia.org/wiki/Comma-separated_values](https://en.wikipedia.org/wiki/Comma-separated_values)
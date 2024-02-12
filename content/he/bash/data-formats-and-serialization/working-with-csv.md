---
title:                "עובדים עם CSV"
aliases:
- /he/bash/working-with-csv/
date:                  2024-02-03T19:19:03.348366-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם קבצי CSV (ערכים מופרדים בפסיקים) ב-Bash היא על אודות עיבוד והתעסקות עם נתונים טבלאיים המאוחסנים בפורמט טקסט פשוט. זה חיוני למתכנתים כיוון שזה מאפשר את האוטומציה של משימות המרה, אנליזה, ואינטגרציה של נתונים ישירות מהשורת הפקודה, ללא הצורך בכלים כבדים יותר או סביבות תכנות.

## איך ל:

**קריאת קובץ CSV שורה אחר שורה**

```bash
while IFS=, read -r column1 column2 column3
do
  echo "עמודה 1: $column1, עמודה 2: $column2, עמודה 3: $column3"
done < sample.csv
```

*דוגמא לפלט:*

```
עמודה 1: id, עמודה 2: name, עמודה 3: email
...
```

**סינון שורות CSV בהתבסס על תנאי**

שימוש ב-`awk`, אתה יכול בקלות לסנן שורות. לדוגמה, למצוא שורות שבהן העמודה השנייה שווה ל-"Alice":

```bash
awk -F, '$2 == "Alice" { print $0 }' sample.csv
```

**שינוי ערך עמודה**

לשנות את העמודה השנייה לאותיות גדולות:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' sample.csv
```

**מיון קובץ CSV בהתבסס על עמודה**

אתה יכול למיין קובץ CSV בהתבסס על, בוא נאמר, העמודה השלישית (באופן מספרי):

```bash
sort -t, -k3,3n sample.csv
```

**שימוש ב-`csvkit` למשימות מורכבות יותר**

`csvkit` הוא אוסף של כלים לשורת הפקודה להמרה ל-CSV ועבודה עם קבצי CSV. ניתן להתקין אותו דרך pip.

להמיר קובץ JSON ל-CSV:

```bash
in2csv data.json > data.csv
```

לשאול קובץ CSV באמצעות SQL:

```bash
csvsql --query "SELECT name FROM sample WHERE id = 10" sample.csv
```

*שימו לב: התקנת `csvkit` דורשת Python וניתן לבצע זאת באמצעות `pip install csvkit`.*

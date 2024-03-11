---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:03.348366-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9\
  \ CSV (\u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD\
  \ \u05D1\u05E4\u05E1\u05D9\u05E7\u05D9\u05DD) \u05D1-Bash \u05D4\u05D9\u05D0 \u05E2\
  \u05DC \u05D0\u05D5\u05D3\u05D5\u05EA \u05E2\u05D9\u05D1\u05D5\u05D3 \u05D5\u05D4\
  \u05EA\u05E2\u05E1\u05E7\u05D5\u05EA \u05E2\u05DD \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05D8\u05D1\u05DC\u05D0\u05D9\u05D9\u05DD \u05D4\u05DE\u05D0\u05D5\u05D7\
  \u05E1\u05E0\u05D9\u05DD \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 \u05D8\u05E7\u05E1\
  \u05D8 \u05E4\u05E9\u05D5\u05D8. \u05D6\u05D4 \u05D7\u05D9\u05D5\u05E0\u05D9 \u05DC\
  \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\
  \u05D6\u05D4\u2026"
lastmod: '2024-03-11T00:14:13.147480-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9 CSV\
  \ (\u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\
  \u05E4\u05E1\u05D9\u05E7\u05D9\u05DD) \u05D1-Bash \u05D4\u05D9\u05D0 \u05E2\u05DC\
  \ \u05D0\u05D5\u05D3\u05D5\u05EA \u05E2\u05D9\u05D1\u05D5\u05D3 \u05D5\u05D4\u05EA\
  \u05E2\u05E1\u05E7\u05D5\u05EA \u05E2\u05DD \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05D8\u05D1\u05DC\u05D0\u05D9\u05D9\u05DD \u05D4\u05DE\u05D0\u05D5\u05D7\u05E1\
  \u05E0\u05D9\u05DD \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 \u05D8\u05E7\u05E1\u05D8\
  \ \u05E4\u05E9\u05D5\u05D8. \u05D6\u05D4 \u05D7\u05D9\u05D5\u05E0\u05D9 \u05DC\u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05D6\
  \u05D4\u2026"
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD CSV"
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

---
title:                "עבודה עם קבצי CSV"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם קובצי CSV היא קריאה וכתיבה של נתונים בפורמט המופרד בפסיקים. תכנתים עובדים עם CSV כדי לייבא ולייצא נתונים בצורה פשוטה ונגישה.

## איך לעשות:
הנה מספר דוגמאות לקריאת וכתיבת נתוני CSV בשורת הפקודה:
קריאת קובץ CSV:
```Bash
while IFS=, read -r column1 column2 column3; do
  echo "תוכן העמודה הראשונה: $column1"
done < input.csv
```

כתיבה לקובץ CSV:
```Bash
echo "עמודה1,עמודה2,עמודה3" > output.csv
```

הדפסת התוצאות בלבד מהעמודה השנייה:
```Bash
cut -d, -f2 input.csv
```

## עיון מעמיק:
CSV הוא פורמט ותיק, נוצר בשנות ה-70 כדרך פשוטה לייצג נתונים טבלאיים. קיימות אלטרנטיבות כמו JSON וXML אך CSV עדיין נפוץ בשל פשטותו. בעת עבודה עם CSV חשוב לטפל במקרים כמו ערכים המכילים פסיקים, שורות חדשות או ציטוטים. 

## ראו גם:
- [RFC 4180](https://tools.ietf.org/html/rfc4180) - המפרט הפורמלי לפורמט CSV.
- [Pandas](https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.read_csv.html) - ספריה ב-Python המאפשרת עבודה נוחה ומתקדמת עם נתוני CSV.

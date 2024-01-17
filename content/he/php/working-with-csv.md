---
title:                "עבודה עם קובץ CSV"
html_title:           "PHP: עבודה עם קובץ CSV"
simple_title:         "עבודה עם קובץ CSV"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/working-with-csv.md"
---

{{< edit_this_page >}}

# איך לעבוד עם CSV ב-PHP: מדריך תכנות לקוראי עברית

## מה זה CSV ולמה מתכנתים עושים את זה?
CSV הוא פורמט קובץ פשוט המאפשר לך לאחסן נתונים בטקסט פשוט, ללא כל סוג של עיצוב או תגיות מיוחדות. הוא נהוג בעיקר בעבודה עם נתונים גדולים, כגון רשימות של פרמטרים או נתונים מובנים. מתכנתים משתמשים ב-CSV כדי לקרוא ולכתוב נתונים בקלות, מבלי להתעסק עם תפריטי משתמש מורכבים או תצוגה מעניינת.

## איך לעבוד עם CSV?
קוד בסיסי עם PHP מאפשר לנו לקרוא ולכתוב לקובץ CSV. להלן דוגמאות של קוד פשוט ופלט המתקבל:

קובץ CSV עם נתונים:

`first_name,last_name,age
John,Smith,25
Jane,Doe,30`

קוד PHP לקריאת הנתונים והדפסתם בטבלה:

```
<?php

// קריאת הקובץ
$file = fopen('file.csv', 'r');

// פעולה על כל שורה בקובץ
while (($data = fgetcsv($file)) !== false) {
  
  // קבלת ערכי השורה והדפסתם בטבלה
  echo '<tr>';
  for ($i = 0; $i < count($data); $i++) {
    echo '<td>' . $data[$i] . '</td>';
  }
  echo '</tr>';
}

// סגירת הקובץ
fclose($file);
```

פלט המתקבל:

```
<table>
  <tr>
    <td>first_name</td>
    <td>last_name</td>
    <td>age</td>
  </tr>
  <tr>
    <td>John</td>
    <td>Smith</td>
    <td>25</td>
  </tr>
  <tr>
    <td>Jane</td>
    <td>Doe</td>
    <td>30</td>
  </tr>
</table>
```

## עומק אית CSV
CSV הינו פורמט קובץ ישן מאוד, ונשמר עד היום כאחד הפורמטים הנפוצים ביותר לעבודה עם נתונים. יתר האפשרויות לעבודה עם נתונים הן להשתמש בבסיסי נתונים או ליצור ממשק משתמש מתאים, אך שימוש ב-CSV יכול לספק פתרון קל ופשוט עבור פרוייקטים קטנים יותר.

כדי לכתוב או לקרוא לקובץ CSV בצורה יעילה יותר, ניתן להשתמש בספריות נוספות כגון "fgetcsv" ו-"fputcsv" המאפשרות לנו לעמוד על ידי פרמטרים נוספים ופעולות עם הנתונים.

למידע נוסף על פעולות נוספות עם CSV ב-PHP ניתן לעיין במקורות המוצעים בסעיף "ראה גם"

## ראה גם
- מדריך תיעוד רשמי לפעולת fgetcsv של PHP: https://www.php.net/manual/en/function.fgetcsv.php
- פרוייקט "league/csv" שמאפשר קריאה וכתיבה לקבצי CSV בעזרת PHP: https://csv.thephpleague.com/
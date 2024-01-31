---
title:                "עבודה עם קבצי CSV"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"

category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם קבצי CSV ב-PHP מאפשרת לך ליצור, לקרוא, ולערוך נתונים בפורמט פשוט. תכניתנים שומרים ומעבדים נתונים בפורמט CSV כי הוא מוכר, נגיש והמרה לטבלאות ב-Excel זה טריוויאלי.

## איך לעשות:
קוד PHP שמדגים יצירה, קריאה ועריכה של קובץ CSV.

יצירת קובץ CSV:
```PHP
<?php
$list = array (
    array('היי', 'זה', 'קובץ', 'CSV'),
    array('שורה', 'שניה', 'עם', 'נתונים'),
);

$fp = fopen('file.csv', 'w');

foreach ($list as $fields) {
    fputcsv($fp, $fields);
}

fclose($fp);
?>
```

קריאת קובץ CSV:
```PHP
<?php
if (($handle = fopen("file.csv", "r")) !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        var_dump($data);
    }
    fclose($handle);
}
?>
```

עריכת קובץ CSV (הוספת שורה):
```PHP
<?php
$list = array (
    array('שורה', 'חדשה', 'לדוגמא', 'CSV'),
);

$fp = fopen('file.csv', 'a'); // 'a' להוספה

foreach ($list as $fields) {
    fputcsv($fp, $fields);
}

fclose($fp);
?>
```

## עיון נוסף:
CSV, ראשי תיבות של Comma-Separated Values, הוא פורמט נתונים מתחילת שנות ה-70. אלטרנטיבות כוללות JSON ו-XML שמציעים גמישות רבה יותר, אך לעיתים הם יכולים להיות יותר מסובכים וקשים לקריאה אנושית. ב-PHP, הפונקציות fgetcsv ו-fputcsv מקלות פעולות עם קבצי CSV.

## ראו גם:
- [PHP: fgetcsv - Manual](https://www.php.net/manual/en/function.fgetcsv.php)
- [PHP: fputcsv - Manual](https://www.php.net/manual/en/function.fputcsv.php)
- [Wikipedia: CSV](https://en.wikipedia.org/wiki/Comma-separated_values)

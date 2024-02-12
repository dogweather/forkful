---
title:                "עובדים עם CSV"
aliases:
- /he/php/working-with-csv/
date:                  2024-02-03T19:21:07.719721-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם CSV (Comma-Separated Values או ערכים מופרדים בפסיקים) כוללת קריאה מקבצי CSV וכתיבה אליהם, פורמט פופולרי לייצוג נתונים טבלאיים בטקסט פשוט. מתכנתים עושים זאת כדי להחליף נתונים בקלות בין תוכניות, מערכות או מסדי נתונים שונים, הודות לפשטותו ותמיכתו הרחבה בפלטפורמות ובשפות תכנות שונות.

## איך לעשות:

PHP מספק פונקציות מובנות לטיפול בקבצי CSV, מה שהופך את הקריאה מהם והכתיבה אליהם לפשוטה ללא צורך בספריות של צד שלישי. הנה דוגמאות להתחלה:

### קריאה מקובץ CSV

ניתן לפתוח קובץ CSV ולקרוא את תכניו באמצעות `fopen()` בשילוב עם `fgetcsv()`:

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "מספר השדות בשורה: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

הסקריפט הזה מדפיס את מספר השדות בכל שורה בעקבות תוכן כל שדה.

### כתיבה לקובץ CSV

כדי לכתוב לקובץ CSV, השתמש ב-`fopen()` במצב כתיבה (`w`) וב-`fputcsv()`:

```php
<?php
$list = [
    ['ID', 'Name', 'Email'],
    [1, 'John Doe', 'john@example.com'],
    [2, 'Jane Doe', 'jane@example.com']
];

$handle = fopen('users.csv', 'w');

foreach ($list as $row) {
    fputcsv($handle, $row);
}

fclose($handle);
?>
```

הסקריפט הזה יוצר קובץ בשם `users.csv` וכותב אליו את הכותרת ושתי שורות נתונים.

### שימוש בספרייה: League\Csv

לטיפול מתקדם יותר ב-CSV, הספרייה `League\Csv` מציעה סט רחב של יכולות. לאחר התקנתה באמצעות Composer (`composer require league/csv`), ניתן להשתמש בה לקריאה וכתיבה של נתוני CSV בצורה גמישה יותר.

#### קריאה עם League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // קבע אם אתה רוצה להשתמש בשורה הראשונה ככותרת

$results = $csv->getRecords();
foreach ($results as $row) {
    print_r($row);
}
?>
```

הסקריפט הזה קורא את `data.csv`, מתייחס לשורה הראשונה כאל כותרות עמודות ומדפיס כל שורה כמערך אסוציאטיבי.

#### כתיבה עם League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', 'Name', 'Email']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "נכתב בהצלחה אל users_new.csv.";
?>
```

זה יוצר את `users_new.csv` וכותב שורת כותרת אחריה שתי שורות נתונים.

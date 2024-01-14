---
title:                "PHP: עבודה עם csv"
simple_title:         "עבודה עם csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/working-with-csv.md"
---

{{< edit_this_page >}}

##למה

 מעוניינים לעסוק בעבודה עם קבצי CSV? כי CSV הוא פורמט פשוט ופופולרי לאחסון נתונים, מה שהופך אותו לקל לעבוד איתם ולעבוד עליהם בקוד.

##כיצד לעשות זאת

אם נרצה לטעון קובץ CSV ולהציג אותו באתר שלנו באמצעות PHP, אנו יכולים לעשות זאת באמצעות הפונקציה "fgetcsv". לדוגמה:

```PHP
$file = fopen('example.csv', 'r');
while (($rowData = fgetcsv($file, 0, ',')) !== FALSE) {
    echo $rowData[0] . ' ' . $rowData[1] . '<br/>';
}
fclose($file);
```

התוכנית הזו מטעינה את קובץ הCSV בשם "example.csv" ומדפיסה כל שורה כטקסט מעוצב באמצעות תגית br ב- HTML. אנו משתמשים בתפקיד השני של הפונקציה "fgetcsv" כדי להגדיר איזה תו נגרר ישמש כנקודת פירוד בין העמודות של הCSV.

##עומק הדיון 

הטמיעת נתונים מקבצי CSV יכולה להיות חסרת תעודה אם יש צורך להתמודד עם קבצים גדולים, במיוחד כאשר צריך לעבוד עם נתונים מעובדים או לעבוד מקובץ CSV גדול במיוחד. כדי לטפל בכך, ניתן להשתמש בפרמטר "memory_limit" בקובץ php.ini כדי להגביל את כמות הזיכרון שמוקצה לביצוע.

בנוסף, כדי להתמודד עם קבצים גדולים, ניתן להשתמש בפונקציות נוספות כמו "fseek" ו-"ftell" כדי לקרוא ולכתוב חלקי קבצים באופן מאורגן.

##ראו גם

- [מדריך על קבצי CSV ב-PHP](https://www.php.net/manual/en/function.fgetcsv.php)
- [תיעוד על הגדרות ה-json ב-PHP](https://www.php.net/manual/en/json.constants.php)
- [פרמטר "memory_limit" ב-PHP.ini](https://www.php.net/manual/en/ini.core.php#ini.memory-limit)
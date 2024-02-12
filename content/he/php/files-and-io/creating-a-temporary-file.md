---
title:                "יצירת קובץ זמני"
aliases:
- /he/php/creating-a-temporary-file.md
date:                  2024-01-20T17:41:58.095287-07:00
model:                 gpt-4-1106-preview
simple_title:         "יצירת קובץ זמני"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני ב-PHP היא דרך להחזיק נתונים באופן זמני במהלך ביצוע הסקריפט. תכניתנים עושים זאת כדי להימנע מניהול קבצים קבועים כאשר רק צורך זמני קיים, לדוגמה בעת עיבוד נתונים גדולים או שמירת מידע רגיש שאינו צריך להישאר במערכת לאחר שימוש.

## איך לעשות: 
קוד PHP עם דוגמאות ופלט לדוגמה:
```PHP
<?php
// יוצרים קובץ זמני
$tempFile = tmpfile();

// כותבים לקובץ
fwrite($tempFile, 'נתונים לדוגמה - Hello World!');

// מקבלים נתיב לקובץ הזמני
$tempPath = stream_get_meta_data($tempFile)['uri'];

// מדפיסים את הנתיב
echo "נתיב הקובץ הזמני: $tempPath\n";

// קוראים מהקובץ
rewind($tempFile); // חוזרים לתחילת הקובץ
echo fread($tempFile, 1024); // קוראים את התוכן

// סוגרים את המעטפת ומוחקים את הקובץ הזמני
fclose($tempFile);
?>
```
פלט הדוגמה:
```
נתיב הקובץ הזמני: /tmp/php3zUxTz
נתונים לדוגמה - Hello World!
```

## עומק המידע
ב-PHP, פונקציית `tmpfile()` מייצרת קובץ זמני מוחזק במערכת הקבצים ואוטומטית מחקה אותו כאשר הקובץ מסגר. היא שימושית במיוחד כשאתה רוצה לעבוד עם קבצים נפרדים מהסביבה שלך ולהבטיח שלא ישארו שאריות לאחר השימוש. היסטורית, PHP התפתח לעבוד עם קבצים באופן מורכב יותר, כאשר תכונות חדשות כמו עיבוד זרם (`stream`) נוספו בגרסאות מתקדמות. חלופות כוללות את השימוש בפונקציות כמו `tempnam()` או `sys_get_temp_dir()` כאשר אתה צריך שם קובץ או מדריך ידוע מראש. למרות זאת, זכור שאתה צריך לנקות כל קובץ זמני שיצרת עם `tempnam()` בעצמך.

## ראו גם
- איך ליצור ולהשתמש בקבצים זמניים ב-PHP:
  [https://www.php.net/manual/en/function.tmpfile.php](https://www.php.net/manual/en/function.tmpfile.php)
- מדריך לניהול קבצים ב-PHP:
  [https://www.php.net/manual/en/book.filesystem.php](https://www.php.net/manual/en/book.filesystem.php)
- דוקומנטציה על פונקציות זמניות ב-PHP:
  [https://www.php.net/manual/en/function.tempnam.php](https://www.php.net/manual/en/function.tempnam.php)

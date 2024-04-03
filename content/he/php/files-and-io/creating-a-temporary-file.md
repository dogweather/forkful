---
date: 2024-01-20 17:41:58.095287-07:00
description: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\
  \u05E0\u05D9 \u05D1-PHP \u05D4\u05D9\u05D0 \u05D3\u05E8\u05DA \u05DC\u05D4\u05D7\
  \u05D6\u05D9\u05E7 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\
  \u05DF \u05D6\u05DE\u05E0\u05D9 \u05D1\u05DE\u05D4\u05DC\u05DA \u05D1\u05D9\u05E6\
  \u05D5\u05E2 \u05D4\u05E1\u05E7\u05E8\u05D9\u05E4\u05D8. \u05EA\u05DB\u05E0\u05D9\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05D4\u05D9\u05DE\u05E0\u05E2 \u05DE\u05E0\u05D9\u05D4\u05D5\
  \u05DC \u05E7\u05D1\u05E6\u05D9\u05DD \u05E7\u05D1\u05D5\u05E2\u05D9\u05DD \u05DB\
  \u05D0\u05E9\u05E8 \u05E8\u05E7 \u05E6\u05D5\u05E8\u05DA \u05D6\u05DE\u05E0\u05D9\
  \ \u05E7\u05D9\u05D9\u05DD,\u2026"
lastmod: '2024-03-13T22:44:39.513120-06:00'
model: gpt-4-1106-preview
summary: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9 \u05D1-PHP \u05D4\u05D9\u05D0 \u05D3\u05E8\u05DA \u05DC\u05D4\u05D7\u05D6\
  \u05D9\u05E7 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF\
  \ \u05D6\u05DE\u05E0\u05D9 \u05D1\u05DE\u05D4\u05DC\u05DA \u05D1\u05D9\u05E6\u05D5\
  \u05E2 \u05D4\u05E1\u05E7\u05E8\u05D9\u05E4\u05D8."
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
weight: 21
---

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

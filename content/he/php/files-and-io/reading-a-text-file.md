---
date: 2024-01-20 17:55:13.744765-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05D6\u05DE\
  \u05E0\u05D9\u05DD \u05E7\u05D3\u05D5\u05DE\u05D9\u05DD, \u05E7\u05E8\u05D9\u05D0\
  \u05EA \u05E7\u05D1\u05E6\u05D9\u05DD \u05D4\u05D9\u05D9\u05EA\u05D4 \u05DE\u05D5\
  \u05E8\u05DB\u05D1\u05EA \u05D9\u05D5\u05EA\u05E8 \u05D5\u05D3\u05E8\u05E9\u05D4\
  \ \u05D4\u05EA\u05E2\u05E1\u05E7\u05D5\u05EA \u05E8\u05D1\u05D4 \u05E2\u05DD \u05DE\
  \u05E9\u05D0\u05D1\u05D9 \u05DE\u05E2\u05E8\u05DB\u05EA. \u05DB\u05D9\u05D5\u05DD\
  , \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05DB\u05DE\u05D5 `file_get_contents()`\
  \ \u05D5`fopen()`, \u05E9\u05DE\u05E9\u05DE\u05E9\u05D5\u05EA\u2026"
lastmod: '2024-04-05T22:50:53.656412-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05D6\u05DE\u05E0\u05D9\u05DD \u05E7\u05D3\u05D5\u05DE\u05D9\u05DD\
  , \u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D1\u05E6\u05D9\u05DD \u05D4\u05D9\u05D9\
  \u05EA\u05D4 \u05DE\u05D5\u05E8\u05DB\u05D1\u05EA \u05D9\u05D5\u05EA\u05E8 \u05D5\
  \u05D3\u05E8\u05E9\u05D4 \u05D4\u05EA\u05E2\u05E1\u05E7\u05D5\u05EA \u05E8\u05D1\
  \u05D4 \u05E2\u05DD \u05DE\u05E9\u05D0\u05D1\u05D9 \u05DE\u05E2\u05E8\u05DB\u05EA\
  ."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 22
---

## איך לעשות:
קריאת קובץ בסיסית:
```PHP
<?php
$content = file_get_contents("example.txt");
echo $content;
?>
```
פלט לדוגמא:
```
Hello, this is the content of the file!
```

באופן מתקדם יותר, קריאת קובץ שורה אחר שורה:
```PHP
<?php
$handle = fopen("example.txt", "r");
if ($handle) {
    while (($line = fgets($handle)) !== false) {
        echo $line;
    }
    fclose($handle);
}
?>
```

גרסת PHP 7.4 ומעלה; קריאה באמצעות arrow function:
```PHP
<?php
$file = new SplFileObject("example.txt");
$file->setFlags(SplFileObject::READ_AHEAD);
foreach ($file as $line) {
    echo $line;
}
?>
```

## הצלילה לפרטים:
בזמנים קדומים, קריאת קבצים הייתה מורכבת יותר ודרשה התעסקות רבה עם משאבי מערכת. כיום, פונקציות כמו `file_get_contents()` ו`fopen()`, שמשמשות לקריאה של קבצים ב-PHP, מספקות דרך קלה ויעילה לעשות את אותו הדבר ללא הטרחה.

ישנן גם אלטרנטיבות לקריאת קבצים כגון `file()` שקוראת את כל השורות לתוך מערך, או ה-class `SplFileObject` שמאפשר שליטה מתקדמת יותר על העיבוד של הקובץ.

דבר נוסף לזכור הוא הטיפול בתקלות; תמיד טוב לבדוק אם הקובץ אכן נפתח בהצלחה לפני שמנסים לעבד אותו, וכנ"ל לסגור אותו לאחר שסיימת לשמוש בו.

## ראה גם:
- [PHP.net on file_get_contents](https://www.php.net/manual/en/function.file-get-contents.php)
- [PHP.net on fopen](https://www.php.net/manual/en/function.fopen.php)
- [PHP.net on SplFileObject](https://www.php.net/manual/en/class.splfileobject.php)
- [PHP The Right Way - Files](https://phptherightway.com/#files)

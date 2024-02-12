---
title:                "קריאת קובץ טקסט"
aliases:
- /he/php/reading-a-text-file.md
date:                  2024-01-20T17:55:13.744765-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## המה ולמה?
קריאת קובץ טקסט ב-PHP זה פשוט לפתוח ולקרוא את התוכן של קובץ טקסט כדי לעבד אותו בתוך התוכנה שלך. תכנתים עושים את זה כדי לטעון נתונים, להגדיר הגדרות ולשמור מידע.

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

---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?

יצירת קובץ זמני היא פעולה שבה המרכזיה מייצרת קובץ שמשמש למשך זמן מוגבל. מתכנתים משתמשים בקבצים זמניים לשמירה על נתונים במהלך הפעלת התוכנית, או לסיוע בתהליכים של ניתוח ובדיקה.


## איך לעשות:

בחשאי, נעבור להסבר בעזרת קטעי קוד ב-PHP. נתחיל בתהליך של יצירת קובץ זמני:

```PHP
<?php
$temp = tmpfile();

fwrite($temp, 'Temporary Data');
rewind($temp);

echo fread($temp, 1024); 
```

בפלט נראה את המחרוזת "Temporary Data". 

## בחינה מעמיקה:

ב-PHP, מתכנתים שולפים את השם של הקובץ הזמני חסר מציאות מהנתיב מערכת ההפעלה באמצעות `sys_get_temp_dir()`.  
ההגדרה של מערכות הפעלה שונות יכולה להשפיע על שם הקובץ הזמני. 
במקרה הרגיל, קבצים זמניים מועדפים, אך לפעמים מתכנתים מעדיפים ליצור ספריית `temp` בספריית האמא של ההפעלה הנוכחית. 

## ראה גם:

1. [PHP: tmpfile - Manual](https://www.php.net/manual/en/function.tmpfile.php)
2. [PHP: sys_get_temp_dir - Manual](https://www.php.net/manual/en/function.sys-get-temp-dir.php)
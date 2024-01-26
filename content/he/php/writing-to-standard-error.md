---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לשגיאת תקן (standard error, STDERR) משמשת להפרדת ההודעות על שגיאות ובעיות מפלט רגיל של תהליך. זה חשוב לניתוח לוגים, דיבאג ומעקב אחרי בעיות, מבלי לערבב עם תוצאות תקניות.

## איך לעשות:
```PHP
<?php
// כתיבה ל-STDERR באמצעות fwrite
fwrite(STDERR, "שגיאה: אירעה תקלה כלשהי.\n");

// כתיבה ל-STDERR באמצעות stream
$stderr = fopen('php://stderr', 'w');
fwrite($stderr, "שגיאה: הבעיה מתמשכת.\n");
fclose($stderr);
?>
```
תוצאה (מוצגת בקונסול):
```
שגיאה: אירעה תקלה כלשהי.
שגיאה: הבעיה מתמשכת.
```

## ניתוח עמוק:
STDERR ב-PHP הוא חלק משלושת הזרם תקשורת הקרויים זרמי תקליטור (streams) - STDIN, STDOUT, ו-STDERR. השימוש ב-STDERR נעשה כחלק מפרדיגמת עיצוב יוניקס. חלופות כוללות כתיבה לקובץ לוג מותאם אישית או שימוש ברישומים (logs) של מערכת ההפעלה. ב-PHP, ניתן לכתוב ל-STDERR ישירות דרך הפונקציה `fwrite` או על ידי פתיחת 'php://stderr' כזרם.

## ראו גם:
- [תיעוד PHP על זרמי תקליטור](https://www.php.net/manual/en/wrappers.php.php)
- [תיעוד PHP על פונקציית `fwrite`](https://www.php.net/manual/en/function.fwrite.php)

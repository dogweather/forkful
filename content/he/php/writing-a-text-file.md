---
title:                "כתיבה לקובץ טקסט"
date:                  2024-01-19
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט ב-PHP זה יצירת קובץ חדש או עדכון קיים עם מידע טקסטואלי. תוכניתנים עושים את זה לשמירת נתונים, לוגים, או תצורות.

## איך לעשות:
```PHP
<?php
// יצירת קובץ טקסט חדש או כתיבה על קובץ קיים
$filename = "example.txt";
$content = "שלום עולם!";

// פותחים את הקובץ לכתיבה
$fp = fopen($filename, "w");

// בדיקה אם הפתיחה הצליחה
if ($fp) {
    fwrite($fp, $content);
    fclose($fp);
    echo "קובץ נשמר.";
} else {
    echo "אירעה שגיאה בפתיחת הקובץ.";
}
?>
```

צפויה תוצאה: `קובץ נשמר.`

## עמוק יותר:
בעבר, כתיבת קבצים הייתה מורכבת יותר, אבל פונקציות PHP כמו fopen(), fwrite(), וfclose() הפשיטו את התהליך. ישנן אלטרנטיבות כגון file_put_contents() שמבצעת את המשימה בשורה אחת. לצד זה, כתיבה לקובץ דורשת ניהול הרשאות קבצים נכון במערכת ההפעלה.

## ראו גם:
- [מדריך לפונקציות קבצים ב-PHP](https://www.php.net/manual/en/book.filesystem.php)
- [תיעוד הפונקציה file_put_contents()](https://www.php.net/manual/en/function.file-put-contents.php)
- [מידע על הרשאות קבצים בלינוקס](https://linux.die.net/man/1/chmod)

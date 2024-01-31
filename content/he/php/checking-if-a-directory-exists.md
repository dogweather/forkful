---
title:                "בדיקה האם תיקייה קיימת"
date:                  2024-01-20T14:58:09.111306-07:00
html_title:           "Gleam: בדיקה האם תיקייה קיימת"
simple_title:         "בדיקה האם תיקייה קיימת"

category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בודקים אם תיקייה קיימת כדי למנוע שגיאות בזמן ריצה של תוכנית ולהבטיח שהפעולות על הקבצים יכולות להתבצע. זה חיוני למיניהל ואבטחת מידע.

## איך לעשות:
ב-php אפשר לבדוק אם תיקייה קיימת באמצעות הפונקציה `is_dir()`:

```php
<?php
$directory = "/path/to/my/directory";

if (is_dir($directory)) {
    echo "התיקייה קיימת!";
} else {
    echo "התיקייה לא קיימת. ייתכן וצריך ליצור אותה?";
}
?>
```

אם התיקייה קיימת, התוצאה תהיה:
```
התיקייה קיימת!
```

אחרת, תראה:
```
התיקייה לא קיימת. ייתכן וצריך ליצור אותה?
```

## Deep Dive
בימים של שרתי קבצים גדולים ואפליקציות מורכבות, הבדיקה שתיקייה קיימת היא לא רק שאלה של נוחות, אלא שאלה של יציבות מערכת. לפני ש`is_dir()` הייתה חלק מהתקן של PHP, מפתחים היו צריכים לנסות ולפתוח תיקייה עם `opendir()` ולכידת שגיאות במקרה של כשל. אלטרנטיבות נוכחיות כוללות שימוש בפונקציות מערכת לבדוק אם תיקייה קיימת מהקונסול, או לשלב ספריות שלישיות שחבילות פונקציונליות נוספות שיודעות להתמודד גם עם סינכרון תיקיות בשרתים מרוחקים.

## ראה גם
- [PHP's `is_dir` official documentation](https://www.php.net/manual/en/function.is-dir.php) - מסמכים רשמיים לפונקציית is_dir.
- [PHP's `file_exists` official documentation](https://www.php.net/manual/en/function.file-exists.php) - מסמך לפונקציה שמבדקת גם אם קובץ קיים, לא רק תיקייה.
- [Stack Overflow: How to check if directory exists in PHP](https://stackoverflow.com/questions/834303/startswith-and-endswith-functions-in-php) - דיונים ופתרונות שונים של מפתחים בקהילה.

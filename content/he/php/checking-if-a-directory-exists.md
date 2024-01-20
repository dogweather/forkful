---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Java: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?

בדיקה אם ספריה קיימת ב-PHP היא פעולה שבה הקוד מאמת אם ספריה מסוימת כבר קיימת על מנת למנוע שגיאות בהשמה כפולה. למתכנתים זה חשוב כדי לוודא שהקוד שלהם לא ייקרוס.

## איך ל: 

ב-PHP, תוכל לבדוק אם ספריה קיימת באמצעות הפעולה `is_dir()`. הנה דוגמא:

```PHP
<?php
$dir = '/path/to/my/dir';

if (is_dir($dir)) {
    echo "הספריה קיימת.";
} else {
    echo "הספריה לא קיימת.";
}
?>
```

אם הספריה קיימת, הקוד יחזיר "הספריה קיימת.".
אם הספריה איננה קיימת, הקוד יחזיר "הספריה לא קיימת.".

## התרסה עמוקה:

נתמך בהטמעת המבנה ההירררכי של המערכת הקובצים, ב-PHP משנת 1997, בגרסה 4. בו בזמן השנים, הושם שיפורים משמעותיים. למשל, במקום להשתמש ב-`is_dir()`, ניתן להשתמש במחלקה `DirectoryIterator` מגרסת PHP 5 והלאה:

```PHP
<?php
$dir = '/path/to/my/dir';

try {
    new DirectoryIterator($dir);
    echo "הספריה קיימת.";
} catch (Exception $e) {
    echo "הספריה לא קיימת.";
}
?>
```

הדרך הזו מגנה על שגיאות מקוד חריג שעשוי לקרות בעת בדיקת הספריה.

## ראה גם: 

אם אתה מעוניין ללמד עוד, תוכל לבדוק את המקורות הבאים:

1. [PHP: is_dir - Manual](https://www.php.net/manual/en/function.is-dir.php)
2. [PHP: DirectoryIterator - Manual](https://www.php.net/manual/en/class.directoryiterator.php) 

המידע שהוצג כאן תיכף מתחיל להרגישו כמו שנייה של הזמן. הבנה מהירה של מידע תיראה כפעולה מינורית.
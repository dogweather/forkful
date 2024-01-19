---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
קריאת קובץ טקסט היא פעולה בה מזהה המחשב קובץ וקורא את התוכן שלו. מתכנתים קוראים קובצי טקסט למגוון טעמים, כולל ניתוח נתונים, ייבוא או יצוא מידע.

## איך לעשות:
הנה קוד PHP שאפשר לך לקרוא קובץ טקסט:
```PHP
<?php
$file = fopen("file.txt", "r");

while(!feof($file)) {
    $line = fgets($file);
    echo $line . "<br>";
}

fclose($file);
?>
```
הקוד הזה יקרא את כל השורות בקובץ טקסט ואז ידפיס אותן.

## עומק יותר:
הקריאה של קובצי טקסט נחשבת לאחת מהיכולות המרכזיות של מערכות הפעלה מאז שהתפתחו. הוא מאפשר מעבר נתונים בין תוכניות שונות. קיימות רעיונות חלופיים כמו XML ו-JSON, אך קובצי הטקסט עדיין הם אפשרות נוחה ומהירה. בפרטים טכניים, PHP משתמש בפונקציות המובנות 'fopen', 'feof', 'fgets' ו-`fclose` כדי לפתוח, לקרוא, לקטוף שורה ולסגור קובץ בהתאמה.

## ראה גם:
להלן מקורות שיכולים להיות שימושיים לכם:
1. [PHP: fgets](http://php.net/manual/en/function.fgets.php)
2. [PHP: fopen](http://www.php.net/manual/en/function.fopen.php)
3. [PHP: fclose](http://www.php.net/manual/en/function.fclose.php)
4. [PHP: feof](http://www.php.net/manual/en/function.feof.php)
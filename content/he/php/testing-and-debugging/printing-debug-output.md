---
title:                "הדפסת פלט לניפוי באגים"
aliases:
- /he/php/printing-debug-output.md
date:                  2024-01-20T17:53:30.856883-07:00
model:                 gpt-4-1106-preview
simple_title:         "הדפסת פלט לניפוי באגים"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
הדפסת פלט לניפוי באגים היא שיטה שבה מתכנתים כותבים מידע מהתכנית לקונסול או לקובץ לצורך ניתוח. זה עוזר לנו לאתר באגים ולהבין את התהליך של התכנית שלנו טוב יותר.

## איך לעשות:
הדפסת פלט ב-PHP די פשוטה. בנקודות חשובות בקוד, אתה רושם משהו כמו:

```PHP
<?php
// בדיקת ערכים
$variable = "משהו מעניין";
echo "בדיקת ערך: $variable";

// דבג של מערך
$array = ["apple", "banana", "cherry"];
print_r($array);

// או כשאנו רוצים לראות פרטי אובייקט
$obj = new StdClass();
$obj->name = "דפנה";
var_dump($obj);
?>
```

הרצת הקוד תפיק בקונסול או בדף האינטרנט:
```
בדיקת ערך: משהו מעניין
Array
(
    [0] => apple
    [1] => banana
    [2] => cherry
)
object(StdClass)#1 (1) {
  ["name"]=>
  string(6) "דפנה"
}
```

## צלילה עמוקה:
ההדפסות לניפוי באגים אינן רק בשביל לראות מה קורה - זו הכרחיות לתהליך הפיתוח. בתחילת ימי המחשבים, המתכנתים שלחו את הפלט להתקני קלט/פלט מכאניים כמו טלטייפ ודפוסיות. היום יש לנו אבזרים מתקדמים יותר כמו xdebug ב-PHP, אשר מאפשרים ניפוי באגים מתקדמים כמו נקודות עצירה וצפייה בערכים בזמן אמת.

כלי נוסף הוא יומן האירועים (logging), שבו הודעות משתמרות בקבצי יומן במקום להיות מודפסות לקונסול - זה מאפשר לך לחזור ולעבור על הנתונים לאחר שהתכנית רצה.

## ראה גם:
- [מדריך ל-Xdebug](https://xdebug.org/docs)
- [מערכת יומנים של PHP](https://www.php.net/manual/en/book.errorfunc.php)
- [מבוא ל-logging ב-PHP](https://www.loggly.com/ultimate-guide/php-logging-basics/)
- [הדרכה ל-var_dump(), print_r() ו-echo](https://www.php.net/manual/en/function.var-dump.php)

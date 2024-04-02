---
date: 2024-01-20 17:53:30.856883-07:00
description: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\
  \u05E4\u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD \u05D4\u05D9\u05D0 \u05E9\u05D9\
  \u05D8\u05D4 \u05E9\u05D1\u05D4 \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DB\
  \u05D5\u05EA\u05D1\u05D9\u05DD \u05DE\u05D9\u05D3\u05E2 \u05DE\u05D4\u05EA\u05DB\
  \u05E0\u05D9\u05EA \u05DC\u05E7\u05D5\u05E0\u05E1\u05D5\u05DC \u05D0\u05D5 \u05DC\
  \u05E7\u05D5\u05D1\u05E5 \u05DC\u05E6\u05D5\u05E8\u05DA \u05E0\u05D9\u05EA\u05D5\
  \u05D7. \u05D6\u05D4 \u05E2\u05D5\u05D6\u05E8 \u05DC\u05E0\u05D5 \u05DC\u05D0\u05EA\
  \u05E8 \u05D1\u05D0\u05D2\u05D9\u05DD \u05D5\u05DC\u05D4\u05D1\u05D9\u05DF \u05D0\
  \u05EA \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05D4\u05EA\u05DB\u05E0\
  \u05D9\u05EA \u05E9\u05DC\u05E0\u05D5 \u05D8\u05D5\u05D1\u2026"
lastmod: '2024-03-13T22:44:39.484255-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD \u05D4\u05D9\u05D0 \u05E9\u05D9\u05D8\
  \u05D4 \u05E9\u05D1\u05D4 \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DB\u05D5\
  \u05EA\u05D1\u05D9\u05DD \u05DE\u05D9\u05D3\u05E2 \u05DE\u05D4\u05EA\u05DB\u05E0\
  \u05D9\u05EA \u05DC\u05E7\u05D5\u05E0\u05E1\u05D5\u05DC \u05D0\u05D5 \u05DC\u05E7\
  \u05D5\u05D1\u05E5 \u05DC\u05E6\u05D5\u05E8\u05DA \u05E0\u05D9\u05EA\u05D5\u05D7\
  . \u05D6\u05D4 \u05E2\u05D5\u05D6\u05E8 \u05DC\u05E0\u05D5 \u05DC\u05D0\u05EA\u05E8\
  \ \u05D1\u05D0\u05D2\u05D9\u05DD \u05D5\u05DC\u05D4\u05D1\u05D9\u05DF \u05D0\u05EA\
  \ \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05D4\u05EA\u05DB\u05E0\u05D9\
  \u05EA \u05E9\u05DC\u05E0\u05D5 \u05D8\u05D5\u05D1\u2026"
title: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD"
weight: 33
---

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

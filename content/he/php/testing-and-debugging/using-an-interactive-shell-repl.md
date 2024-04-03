---
date: 2024-01-26 04:17:38.857176-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05D4\u05E4\u05E2\u05DC \u05D0\u05EA \u05D4\
  -REPL \u05E9\u05DC PHP \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D4\u05E8\u05E6\u05EA\
  \ `php -a` \u05D1\u05D8\u05E8\u05DE\u05D9\u05E0\u05DC \u05E9\u05DC\u05DA. \u05D4\
  \u05E0\u05D4 \u05D8\u05E2\u05D9\u05DE\u05D4 \u05E9\u05DC \u05D0\u05D9\u05DA \u05D6\
  \u05D4 \u05E2\u05D5\u05D1\u05D3."
lastmod: '2024-03-13T22:44:39.482642-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E4\u05E2\u05DC \u05D0\u05EA \u05D4-REPL \u05E9\u05DC PHP \u05E2\
  \u05DC \u05D9\u05D3\u05D9 \u05D4\u05E8\u05E6\u05EA `php -a` \u05D1\u05D8\u05E8\u05DE\
  \u05D9\u05E0\u05DC \u05E9\u05DC\u05DA."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
weight: 34
---

## איך ל:
הפעל את ה-REPL של PHP על ידי הרצת `php -a` בטרמינל שלך. הנה טעימה של איך זה עובד:

```php
php > echo "Hello, World!";
Hello, World!
php > $arr = [1, 2, 3];
php > print_r($arr);
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

ניתן גם להגדיר פונקציות:

```php
php > function sum($a, $b) { return $a + $b; }
php > echo sum(5, 10);
15
```

## צלילה עמוקה
REPLs קיימים בצורה כזו או אחרת מתחילת ימי ה-LISP בשנות ה-60. קונסולה אינטרקטיבית של PHP פחות מתקדמת בהשוואה לשפות כמו Python או JavaScript. היא לא שומרת מצב בין הפעלות וחסרות לה תכונות כמו השלמה אוטומטית. לקבלת REPL של PHP עם יותר תכונות, שקול אלטרנטיבות כמו `psysh` או `boris`. קונסולות אלו של גורמים שלישיים מציעות כלים טובים יותר להבנת הקוד, השלמת טאב ואף מנגנון דיבאג.

מאחורי הקלעים, REPL של PHP עובדת על ידי הידור וביצוע כל שורת קוד ככל שהיא נכנסת. המגבלות של גישה זו הופכות לברורות עם דברים כמו הצהרה מחדש על מחלקות, שאינה אפשרית באותה הפעלה. זה מעולה לבדיקות פשוטות אך יכול להיות מסורבל למשימות מורכבות יותר.

## ראה גם
- [מדריך PHP - קונסולה אינטרקטיבית](https://www.php.net/manual/en/features.commandline.interactive.php)
- [PsySH: קונסולת מפתח זמן ריצה, דיבאגר אינטרקטיבי ו-REPL עבור PHP](https://psysh.org/)
- [Boris: REPL קטנטן עבור PHP](https://github.com/borisrepl/boris)

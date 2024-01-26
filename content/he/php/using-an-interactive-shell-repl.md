---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
date:                  2024-01-26T04:17:38.857176-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## מה ולמה?
קונסולה אינטרקטיבית, או REPL (Read-Eval-Print Loop, לולאת קריאה-הערכה-הדפסה), מאפשרת לך לכתוב ולרוץ קוד PHP בזמן אמת. היא אידיאלית לניסויים, דיבאגינג או למידה, כאשר אתה יכול לבדוק קטעי קוד בלי הצורך ביצירת סקריפט מלא.

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
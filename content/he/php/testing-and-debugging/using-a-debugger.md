---
title:                "שימוש בדיבאגר"
aliases: - /he/php/using-a-debugger.md
date:                  2024-01-26T04:09:39.164036-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בדיבאגר"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/using-a-debugger.md"
---

{{< edit_this_page >}}

## מה ולמה?
מנגנון דיבאג הוא כלי אשר עוזר למתכנתים להבין מה קוד התוכנה שלהם באמת עושה בזמן הריצה. זהו הזכוכית המגדלת שמאפשרת לנו להתמקד בבאגים - הבעיות המציקות הגורמות לתוכניות שלנו לקרוס או להפליט תשובות שגויות - ולדכאן אותם. אנו משתמשים במנגנוני דיבאג מפני שהם חוסכים לנו שעות של הדפסות ומשחקי ניחושים.

## איך ל:
PHP מגיע עם מנגנון דיבאג אינטראקטיבי בשם Xdebug. הנה איך להשתמש בו.

ראשית, וודא ש-Xdebug מותקן ומוגדר בקובץ `php.ini` שלך:

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

לאחר מכן, כתוב סקריפט PHP פשוט עם באג:

```PHP
<?php
function add($a, $b) {
    return $a - $b; // אופס! זה אמור להיות פלוס, לא מינוס
}

$result = add(1, 2);
echo "התוצאה היא: $result"; // התוצאה אמורה להיות 3, לא -1
```

באמצעות IDE כמו PhpStorm, הגדר נקודת עצירה על ידי לחיצה ליד מספר השורה. הרץ את המנגנון דיבאג וצפה איך המשתנים משתנים ככל שאתה עובר דרך הביצוע. כשתדלג מעל פונקציית ה`add`, תבחין ש-`$result` הופך ל-1-, מה שלא צפוי.

## צלילה עמוקה:
באופן היסטורי, PHP נמצא בשימוש בעיקר לתסריטים קטנים, ופיתוח נעשה בעיקר על ידי הוספת הצהרות `var_dump()` ו-`print_r()` לאורך הקוד. עם הזמן, עם PHP הפך לשחקן מרכזי בפיתוח אתרים, כלים מתוחכמים יותר כמו Xdebug ו-Zend Debugger החלו להיכנס לשימוש.

בראש רשימת החלופות ל-Xdebug נמצאים pcov ו-phpdbg. אלו מציעים מאפיינים שונים אך ייתכן שלא יהיו מקיפים כמו Xdebug. phpdbg הוא מנגנון דיבאג קל וספציפי ל-PHP, המסופק עם PHP החל מגרסה 5.6, ו-pcov הוא מנהל הכיסוי קוד.

בעת יישום מנגנון דיבאג, זכור שלעולם לא תשאיר את המנגנון דיבאג פועל בשרת הייצור שלך, מכיוון שהוא עלול לחשוף פגיעויות אבטחה ולהאט את הביצועים.

## ראה גם:
- [תיעוד Xdebug](https://xdebug.org/docs/)
- [מדריך לדיבאג ב-PhpStorm](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net על phpdbg](https://www.php.net/manual/en/book.phpdbg.php)
- [pcov ב-GitHub](https://github.com/krakjoe/pcov)

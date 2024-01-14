---
title:                "PHP: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/writing-tests.md"
---

{{< edit_this_page >}}

"למה"
יצירת בדיקות בPHP היא כלי חיוני למתכנתים כדי לוודא שהקוד שהם כותבים עובד כפי שצריך. בדיקות עוזרות לזהות באופן נמצא באגים ולקבוע שהפונקציות שנכתבו עובדות כפי שצריך לפני שהן משוחררות לייצור.

"כיצד לכתוב בדיקות בPHP"
הפנים הבאות יעזרו לך להתחיל עם כתיבת בדיקות בPHP בצורה נכונה ויעילה.

```PHP
// דוגמא לפונקציה שבודקת אם המספר הנתון הוא זוגי
function isEven($number) {
  if($number % 2 == 0) {
    return true;
  } else {
    return false;
  }
}

//בדיקת בדיקה
echo isEven(4); // יחזיר true
echo isEven(7); // יחזיר false
```

"עומק הצוללת"
כדי לכתוב בדיקות בצורה נכונה, יש להישתמש בכמה טכניקות מתקדמות כמו עקביות, מודולריות והשלמת שורות קוד. כמו כן, יש לוודא שהבדיקות מכסות כל פעולה ונתונים שונים בקוד.

"ראה גם"
- מדריך לבדיקות בPHP: https://www.php.net/manual/en/book.simpletest.php
- עקרונות חשובים לכתיבה נכונה של בדיקות בPHP: https://www.guru99.com/phpunit-testing.html
- מדריך לכתיבת בדיקות בPHP באמצעות PHPUnit: https://www.tutorialspoint.com/phpunit/index.htm
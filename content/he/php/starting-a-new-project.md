---
title:                "התחלת פרויקט חדש"
aliases:
- he/php/starting-a-new-project.md
date:                  2024-01-20T18:04:22.422583-07:00
model:                 gpt-4-1106-preview
simple_title:         "התחלת פרויקט חדש"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?

פתיחת פרויקט חדש ב-PHP זה תהליך של הקמת סביבת פיתוח מתאימה לבניית משהו חדש. תכנתים עושים זאת כדי ליצור מוצרים חדשים, ללמוד, ולפתור בעיות.

## איך לעשות:

כדי להתחיל, תצטרך להגדיר בסיס לפרויקט שלך. בואו נראה איך אפשר להקים פרויקט PHP פשוט.

```PHP
<?php
// התחברות למערכת מנהל החבילות Composer.
require 'vendor/autoload.php';

// יצירת קובץ index.php.
echo 'שלום עולם!';

// אל תשכח להריץ: composer install
?>
```
פלט לדוגמה:
```
שלום עולם!
```

## עיון מעמיק:

Composer הוא מנהל חבילות פופולרי ב-PHP, שמשמש לניהול תלותיות של פרויקט. פותח ב-2012, והוא חלק בלתי נפרד מהקהילה. ישנם אלטרנטיבות, כמו PEAR, אבל Composer הוא הבחירה העכשווית. כשאתה מתחיל פרויקט חדש, חשוב להבין איך להשתמש בממשק הפקודות של Composer ואיך לטפל בקובץ `composer.json`. וגם, זה סופר חשוב לקיים רגלי אבטחה טובים, דוגמת יישום CSRF ו-XSS.

## נוסף על כך:

- [PHP The Right Way](https://phptherightway.com/) - מדריך עכשווי ומלא רעיונות ל PHP.
- [Composer](https://getcomposer.org/) - האתר הרשמי של מנהל החבילות Composer.
- [Packagist](https://packagist.org/) - האוסף חבילות הרשמי של Composer.
- [PHP Fig](https://www.php-fig.org/) - קבוצת PHP Framework Interop Group שמוציאה את PSR, כללים לכתיבת קוד PHP בצורה סטנדרטית.

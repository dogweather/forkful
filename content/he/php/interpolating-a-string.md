---
title:                "שרבוב מחרוזת"
date:                  2024-01-20T17:51:30.073422-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
אינטרפולציה של מחרוזת ב-PHP מאפשרת לשלב משתנים ישירות בתוך מחרוזת. זה נעשה כי זה מייעל את הכתיבה וקורא את הקוד – פשוט יותר ומהיר יותר.

## איך לעשות:
```PHP
$name = 'דוד';
$greeting = "שלום, $name!";
echo $greeting; // ידפיס: שלום, דוד!
```

עוד דוגמה, עם תחביר מורכב יותר:
```PHP
$item = 'כובע';
$price = 20;
echo "המחיר של ה$item הוא: {$price} ש"ח"; // ידפיס: המחיר של הכובע הוא: 20 ש"ח
```

## עיון נוסף
בעבר, היינו צריכים לשלב מחרוזות ומשתנים בעזרת הפעולה `.` לצרפם יחד, אבל החל מ-PHP 5.0, אינטרפולציה הפכה להיות עניין פשוט יותר. יש גם אפשרות לעשות את זה בעזרת פונקציות למחרוזות כמו `sprintf` או `printf`. כשאנחנו משתמשים בתחביר מורכב או בתוך מחרוזת עם מרכאות כפולות, פי.אח.פי. מחפשת את המשתנה ומחליפה אותו בערך שלו.

## ראה גם
- [PHP: מחרוזות](https://www.php.net/manual/en/language.types.string.php)
- [PHP: משתנים משולבים במחרוזות](https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing)
- [PHP: sprintf](https://www.php.net/manual/en/function.sprintf.php)

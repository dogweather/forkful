---
title:                "כתיבה לשגיאה התקנית"
aliases: - /he/php/writing-to-standard-error.md
date:                  2024-02-03T19:35:03.529897-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבה לשגיאה התקנית"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

כתיבה לפלט השגיאה הסטנדרטי (stderr) ב-PHP היא עניין של הכוונת הודעות שגיאה או אבחון בנפרד מהפלט הסטנדרטי (stdout), מה שמאפשר למפתחים לנהל בצורה טובה יותר את זרמי הפלט שלהם לצורך דיבאגינג ורישום. תכניתנים משתמשים בטכניקה זו על מנת להבטיח שהודעות השגיאה לא מתערבות עם פלט התוכנית, מה שמקל על פיקוח ואיתור בעיות באפליקציות.

## איך לעשות זאת:

ב-PHP, ניתן להשיג כתיבה ל-stderr באמצעות השימוש בפונקציה `fwrite()` יחד עם הקבוע המוגדר מראש `STDERR`, שמייצג את זרם פלט השגיאות.

```php
<?php
// כתיבת הודעה פשוטה ל-stderr.
fwrite(STDERR, "זוהי הודעת שגיאה.\n");
```

פלט לדוגמא כאשר הסקריפט מופעל מהשורת הפקודה:
```
זוהי הודעת שגיאה.
```

כדי להדגים שימוש מעשי יותר, נחשוב על תרחיש בו אתם פוענחים קלט מהמשתמש ונתקלים בנתונים בלתי צפויים:
```php
<?php
$input = 'נתונים בלתי צפויים';

// סימולציה של שגיאה בעיבוד קלט מהמשתמש.
if ($input === 'נתונים בלתי צפויים') {
    fwrite(STDERR, "שגיאה: נתקל בקלט בלתי צפוי.\n");
    exit(1); // יציאה עם ערך שאינו אפס כדי להצביע על שגיאה.
}
```

למרות שהיכולות הקיימות ב-PHP לטיפול ב-stderr בדרך כלל מספיקות, כאשר מתמודדים עם אפליקציות מורכבות יותר או כאשר רוצים לשלב רישום של stderr עם מערכות חיצוניות, ספריות צד שלישי כמו Monolog יכולות להיות בעלות ערך רב. Monolog היא ספריית רישום שיכולה לטפל ב-stderr בין כמה אפשרויות אחרות (קבצים, סוקטים וכו').

שימוש ב-Monolog לכתיבה ל-stderr:

ראשית, ודא שהתקנת את Monolog באמצעות Composer:
```
composer require monolog/monolog
```

לאחר מכן, תוכלו להגדיר את Monolog להשתמש ב-`StreamHandler` המכוון אל `php://stderr`:

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// יצירת ערוץ רישום
$log = new Logger('name');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// הוספת הודעת רישום ל-stderr
$log->warning('זוהי הודעת אזהרה.');
```

הקוד לעיל משתמש ב-Monolog כדי לשלוח הודעת אזהרה ל-stderr, דבר שמועיל במיוחד לאפליקציות שדורשות תצורות רישום מפורטות או פיקוח חיצוני על הרישום.

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:35:03.529897-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05D1-PHP, \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05D9\u05D2 \u05DB\u05EA\
  \u05D9\u05D1\u05D4 \u05DC-stderr \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\
  \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4\
  \ `fwrite()` \u05D9\u05D7\u05D3 \u05E2\u05DD \u05D4\u05E7\u05D1\u05D5\u05E2 \u05D4\
  \u05DE\u05D5\u05D2\u05D3\u05E8 \u05DE\u05E8\u05D0\u05E9 `STDERR`, \u05E9\u05DE\u05D9\
  \u05D9\u05E6\u05D2 \u05D0\u05EA \u05D6\u05E8\u05DD \u05E4\u05DC\u05D8 \u05D4\u05E9\
  \u05D2\u05D9\u05D0\u05D5\u05EA."
lastmod: '2024-03-13T22:44:39.508223-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-PHP, \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05D9\u05D2 \u05DB\
  \u05EA\u05D9\u05D1\u05D4 \u05DC-stderr \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA\
  \ \u05D4\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\
  \u05D4 `fwrite()` \u05D9\u05D7\u05D3 \u05E2\u05DD \u05D4\u05E7\u05D1\u05D5\u05E2\
  \ \u05D4\u05DE\u05D5\u05D2\u05D3\u05E8 \u05DE\u05E8\u05D0\u05E9 `STDERR`, \u05E9\
  \u05DE\u05D9\u05D9\u05E6\u05D2 \u05D0\u05EA \u05D6\u05E8\u05DD \u05E4\u05DC\u05D8\
  \ \u05D4\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA."
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
weight: 25
---

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

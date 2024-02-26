---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:14.022374-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1-PHP \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05D4\
  \ \u05D0\u05D5 \u05E4\u05EA\u05D9\u05D7\u05D4 \u05E9\u05DC \u05E7\u05D5\u05D1\u05E5\
  \ \u05D5\u05D4\u05DB\u05E0\u05E1\u05EA \u05EA\u05D5\u05DB\u05DF \u05D0\u05DC\u05D9\
  \u05D5. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DE\u05E8 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD, \u05DB\u05DE\u05D5 \u05EA\u05D5\u05DB\u05DF \u05E9\u05E0\u05D5\
  \u05E6\u05E8 \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D4\u05DE\u05E9\u05EA\u05DE\u05E9\
  \ \u05D0\u05D5 \u05D9\u05D5\u05DE\u05E0\u05D9 \u05E8\u05D9\u05E9\u05D5\u05DD, \u05DE\
  \u05E2\u05D1\u05E8\u2026"
lastmod: '2024-02-25T18:49:37.751634-07:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D1-PHP \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05D4\
  \ \u05D0\u05D5 \u05E4\u05EA\u05D9\u05D7\u05D4 \u05E9\u05DC \u05E7\u05D5\u05D1\u05E5\
  \ \u05D5\u05D4\u05DB\u05E0\u05E1\u05EA \u05EA\u05D5\u05DB\u05DF \u05D0\u05DC\u05D9\
  \u05D5. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DE\u05E8 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD, \u05DB\u05DE\u05D5 \u05EA\u05D5\u05DB\u05DF \u05E9\u05E0\u05D5\
  \u05E6\u05E8 \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D4\u05DE\u05E9\u05EA\u05DE\u05E9\
  \ \u05D0\u05D5 \u05D9\u05D5\u05DE\u05E0\u05D9 \u05E8\u05D9\u05E9\u05D5\u05DD, \u05DE\
  \u05E2\u05D1\u05E8\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט ב-PHP כוללת יצירה או פתיחה של קובץ והכנסת תוכן אליו. מתכנתים עושים זאת כדי לשמר נתונים, כמו תוכן שנוצר על ידי המשתמש או יומני רישום, מעבר למחזור החיים של התוכנית.

## איך לעשות זאת:
PHP תומכת מהבסיס בכתיבת קבצים דרך פונקציות כמו `file_put_contents`, `fopen` יחד עם `fwrite`, ו-`fclose`. הנה איך להשתמש בהן:

### כתיבה פשוטה עם `file_put_contents`:
פונקציה זו מפשטת את התהליך של כתיבה לקובץ על ידי ביצוע הכל בשלב אחד.
```php
$content = "שלום, עולם!";
file_put_contents("hello.txt", $content);
// בודק אם הקובץ נכתב בהצלחה
if (file_exists("hello.txt")) {
    echo "הקובץ נוצר בהצלחה!";
} else {
    echo "נכשל ביצירת הקובץ.";
}
```

### כתיבה מתקדמת עם `fopen`, `fwrite`, ו-`fclose`:
לשליטה רבה יותר על הכתיבה לקובץ, כמו הוספת טקסט או יותר טיפול בשגיאות, השתמשו ב-`fopen` עם `fwrite`.
```php
$file = fopen("hello.txt", "a"); // מצב 'a' להוספה, 'w' לכתיבה
if ($file) {
    fwrite($file, "\nהוספת עוד תוכן.");
    fclose($file);
    echo "תוכן נוסף בהצלחה!";
} else {
    echo "נכשל בפתיחת הקובץ.";
}
```

#### קריאת הקובץ לצורך פלט:
כדי לאמת את התוכן שלנו:
```php
echo file_get_contents("hello.txt");
```
**פלט לדוגמא:**
```
שלום, עולם!
הוספת עוד תוכן.
```

### שימוש בספריות צד שלישי:
לפעולות עם קבצים יותר מורכבות, ניתן להשתמש בספריות כמו `League\Flysystem` שמספקות שכבת הפשטה מעל מערכת הקבצים, אך פונקציות המובנות של PHP לעיתים קרובות מספיקות למשימות הכתיבה הבסיסיות בקבצים. הנה דוגמה קצרה במידה ותבחרו לחקור את `Flysystem`:
```php
require 'vendor/autoload.php';
use League\Flysystem\Filesystem;
use League\Flysystem\Local\LocalFilesystemAdapter;

$adapter = new LocalFilesystemAdapter(__DIR__);
$filesystem = new Filesystem($adapter);

$filesystem->write('hello.txt', "משתמשים ב-Flysystem לכתיבת זה.");
```
הדוגמה מניחה שהתקנתם את `league/flysystem` דרך Composer. ספריות צד שלישי יכולות לפשט באופן משמעותי פעולות טיפול בקבצים יותר מורכבות, במיוחד כאשר עובדים עם מערכות אחסון שונות בצורה חלקה.

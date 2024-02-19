---
aliases:
- /he/php/logging/
date: 2024-01-26 01:07:03.386482-07:00
description: "\u05DC\u05D5\u05D2\u05D9\u05DD \u05D4\u05DD \u05D1\u05E2\u05E6\u05DD\
  \ \u05D3\u05D5\u05DE\u05D9\u05DD \u05DC\u05E9\u05DE\u05D9\u05E8\u05EA \u05D9\u05D5\
  \u05DE\u05DF \u05E2\u05D1\u05D5\u05E8 \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA\
  ; \u05D6\u05D4\u05D5 \u05D4\u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05DC \u05EA\u05D9\
  \u05E2\u05D5\u05D3 \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD, \u05E9\u05D2\u05D9\
  \u05D0\u05D5\u05EA, \u05D5\u05E0\u05E7\u05D5\u05D3\u05D5\u05EA \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D7\u05E9\u05D5\u05D1\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\
  \u05EA \u05E9\u05E7\u05D5\u05E8\u05D5\u05EA \u05DB\u05D0\u05E9\u05E8 \u05D4\u05D0\
  \u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\u05D4 \u05E9\u05DC\u05DA \u05E8\u05E6\u05D4\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
lastmod: 2024-02-18 23:08:52.938644
model: gpt-4-1106-preview
summary: "\u05DC\u05D5\u05D2\u05D9\u05DD \u05D4\u05DD \u05D1\u05E2\u05E6\u05DD \u05D3\
  \u05D5\u05DE\u05D9\u05DD \u05DC\u05E9\u05DE\u05D9\u05E8\u05EA \u05D9\u05D5\u05DE\
  \u05DF \u05E2\u05D1\u05D5\u05E8 \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA; \u05D6\
  \u05D4\u05D5 \u05D4\u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05DC \u05EA\u05D9\u05E2\
  \u05D5\u05D3 \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD, \u05E9\u05D2\u05D9\u05D0\
  \u05D5\u05EA, \u05D5\u05E0\u05E7\u05D5\u05D3\u05D5\u05EA \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05D7\u05E9\u05D5\u05D1\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA\
  \ \u05E9\u05E7\u05D5\u05E8\u05D5\u05EA \u05DB\u05D0\u05E9\u05E8 \u05D4\u05D0\u05E4\
  \u05DC\u05D9\u05E7\u05E6\u05D9\u05D4 \u05E9\u05DC\u05DA \u05E8\u05E6\u05D4. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
title: "\u05E8\u05D9\u05E9\u05D5\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA (\u05DC\
  \u05D5\u05D2\u05D9\u05DD)"
---

{{< edit_this_page >}}

## מה ולמה?

לוגים הם בעצם דומים לשמירת יומן עבור הקוד שלך; זהו הפעולה של תיעוד אירועים, שגיאות, ונקודות נתונים חשובות אחרות שקורות כאשר האפליקציה שלך רצה. מתכנתים עושים את זה כדי לעקוב אחר מה שקורה מתחת למכסה, לאבחן בעיות, ולשמור על רצף ביקורת לניתוח או ענייני תאימות בעתיד.

## איך לעשות:

ל-PHP יש פונקציה מובנית לתיעוד שגיאות שקל להשתמש בה. פשוט הכנס `error_log()` לתוך הקוד שלך כדי לשלוח הודעה ללוגים של השרת. אתה יכול גם להתאימו כך שירשום לקובץ מסוים.

```php
<?php
// תיעוד הודעה פשוטה של מידע
error_log("This is an info log entry.");

// תיעוד הודעת שגיאה
error_log("This is an error log entry.", 0);

// תיעוד לקובץ מסוים
file_put_contents('/path/to/your/custom.log', "A custom log entry.\n", FILE_APPEND);

// שימוש ב-Monolog עבור תיעוד מובנה
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// יצירת המתעד
$logger = new Logger('name');
// עכשיו הוסף כמה מטפלים
$logger->pushHandler(new StreamHandler('/path/to/your/monolog.log', Logger::WARNING));

// עכשיו אתה יכול להשתמש במתעד שלך
$logger->warning('This is a warning log!');
$logger->error('This is an error log!');
?>
```

זה יוציא את הלוגים שלך ללוג של השרת או לקובץ שבחרת בפורמט טקסט פשוט.

## צלילה עמוקה:

בעבר, תכנתי PHP הסתמכו על הפונקציה `error_log()` או על לוגי Apache/Nginx כדי לתפוס בעיות, אבל זה יכול להיות כאוטי עם הצורך לנתח קבצי טקסט פשוטים ואין דרך קלה לסנן או למיין אותם. כאן נכנסים ספריות תיעוד כמו Monolog, שהביאו את תקופת התיעוד המובנה ב-PHP. פתרונות אלה נותנים לך שליטה טובה יותר על ידי הצעת ערוצי תיעוד מרובים, רמות חומרה, ופלט מעוצב (כמו JSON, שהוא חלום לניתוח תוכניתי).

חלופות ל-Monolog כוללות את Log4php, KLogger, ו-Log4php של Apache. מבחינת היישום, תיעוד אמין דורש לא רק לזרוק נתונים לכל מקום, אלא לשקול דברים כמו סיבוב יומנים, אסטרטגיות ארכיון, ואינטגרציה עם כלי מעקב כדי להיות מועיל באמת.

עליך לקחת בחשבון את [ממשק ה-PSR-3 Logger](https://www.php-fig.org/psr/psr-3/), שמגדיר ממשק נפוץ עבור ספריות תיעוד, מבטיח התאמה ודרך עקבית לגשת למנגנוני תיעוד.

## ראה גם:

- [מאגר Monolog ב-GitHub](https://github.com/Seldaek/monolog)
- [מפרט ממשק ה-PSR-3 Logger](https://www.php-fig.org/psr/psr-3/)
- [תיעוד PHP Error Log](https://www.php.net/manual/en/function.error-log.php)
- [KLogger: מחלקת תיעוד פשוטה עבור PHP](https://github.com/katzgrau/KLogger)
- [Log4php: פריימוורק תיעוד גמיש עבור PHP](https://logging.apache.org/log4php/)

התחל לרטוב את רגלייך עם הפונקציות המובנות, אבל לגישה יותר ניתנת לתחזוקה ולקנה מידה, שקול להשקיע זמן כדי להתרגל עם ספרייה כמו Monolog. תיעוד מהנה!

---
date: 2024-01-26 01:07:03.386482-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DC-PHP \u05D9\
  \u05E9 \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\
  \u05EA \u05DC\u05EA\u05D9\u05E2\u05D5\u05D3 \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05E9\u05E7\u05DC \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D4. \u05E4\u05E9\
  \u05D5\u05D8 \u05D4\u05DB\u05E0\u05E1 `error_log()` \u05DC\u05EA\u05D5\u05DA \u05D4\
  \u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DC\u05D5\
  \u05D7 \u05D4\u05D5\u05D3\u05E2\u05D4 \u05DC\u05DC\u05D5\u05D2\u05D9\u05DD \u05E9\
  \u05DC \u05D4\u05E9\u05E8\u05EA. \u05D0\u05EA\u05D4 \u05D9\u05DB\u05D5\u05DC \u05D2\
  \u05DD \u05DC\u05D4\u05EA\u05D0\u05D9\u05DE\u05D5\u2026"
lastmod: '2024-03-13T22:44:39.491137-06:00'
model: gpt-4-1106-preview
summary: "\u05DC-PHP \u05D9\u05E9 \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 \u05DE\
  \u05D5\u05D1\u05E0\u05D9\u05EA \u05DC\u05EA\u05D9\u05E2\u05D5\u05D3 \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA \u05E9\u05E7\u05DC \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9\
  \ \u05D1\u05D4."
title: "\u05E8\u05D9\u05E9\u05D5\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA (\u05DC\
  \u05D5\u05D2\u05D9\u05DD)"
weight: 17
---

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

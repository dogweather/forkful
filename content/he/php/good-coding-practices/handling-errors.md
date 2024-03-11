---
date: 2024-01-26 00:56:36.268616-07:00
description: "\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3\u05D5\u05EA \u05E2\u05DD \u05E9\
  \u05D2\u05D9\u05D0\u05D5\u05EA \u05D1-PHP \u05DE\u05D3\u05D5\u05D1\u05E8\u05EA \u05D1\
  \u05E0\u05D9\u05D4\u05D5\u05DC \u05D5\u05D4\u05D2\u05D9\u05D1\u05D4 \u05DC\u05EA\
  \u05E0\u05D0\u05D9\u05DD \u05E9\u05DE\u05E4\u05E8\u05D9\u05E2\u05D9\u05DD \u05DC\
  \u05D6\u05E8\u05D9\u05DE\u05D4 \u05D4\u05E8\u05D2\u05D9\u05DC\u05D4 \u05E9\u05DC\
  \ \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA, \u05DB\u05DE\u05D5 \u05E7\u05D1\u05E6\u05D9\
  \u05DD \u05D7\u05E1\u05E8\u05D9\u05DD \u05D0\u05D5 \u05E7\u05DC\u05D8 \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05E9\u05D2\u05D5\u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DE\u05EA\u05DE\u05D5\u05D3\u05D3\u05D9\u05DD \u05E2\u05DD \u05E9\
  \u05D2\u05D9\u05D0\u05D5\u05EA \u05DB\u05D3\u05D9\u2026"
lastmod: '2024-03-11T00:14:12.964392-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3\u05D5\u05EA \u05E2\u05DD \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA \u05D1-PHP \u05DE\u05D3\u05D5\u05D1\u05E8\u05EA \u05D1\u05E0\
  \u05D9\u05D4\u05D5\u05DC \u05D5\u05D4\u05D2\u05D9\u05D1\u05D4 \u05DC\u05EA\u05E0\
  \u05D0\u05D9\u05DD \u05E9\u05DE\u05E4\u05E8\u05D9\u05E2\u05D9\u05DD \u05DC\u05D6\
  \u05E8\u05D9\u05DE\u05D4 \u05D4\u05E8\u05D2\u05D9\u05DC\u05D4 \u05E9\u05DC \u05EA\
  \u05D5\u05DB\u05E0\u05D9\u05EA, \u05DB\u05DE\u05D5 \u05E7\u05D1\u05E6\u05D9\u05DD\
  \ \u05D7\u05E1\u05E8\u05D9\u05DD \u05D0\u05D5 \u05E7\u05DC\u05D8 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05E9\u05D2\u05D5\u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DE\u05EA\u05DE\u05D5\u05D3\u05D3\u05D9\u05DD \u05E2\u05DD \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA \u05DB\u05D3\u05D9\u2026"
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
התמודדות עם שגיאות ב-PHP מדוברת בניהול והגיבה לתנאים שמפריעים לזרימה הרגילה של תוכנית, כמו קבצים חסרים או קלט נתונים שגוי. מתכנתים מתמודדים עם שגיאות כדי למנוע קריסות ולתת למשתמשים חוויה חלקה יותר.

## איך לעשות:
ב-PHP, אתה יכול לנהל שגיאות באמצעות בלוקים של `try-catch`, ותוכל להתאים אישית את התהליך עם מטפלי שגיאות מותאמים אישית וזריקת חריגות (exceptions).

```php
// דוגמה בסיסית של try-catch
try {
  // עשה משהו מסוכן
  $file = fopen("nonexistentfile.txt", "r");
} catch (Exception $e) {
  // טפל בשגיאה
  echo "Error: " . $e->getMessage();
}

// הגדרת מטפל שגיאות מותאם אישית
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// שימוש בחריגות
class MyException extends Exception {}

try {
  // עשה משהו וזרוק חריגה מותאמת אישית
  throw new MyException("Custom error!");
} catch (MyException $e) {
  // טפל בחריגה המותאמת אישית
  echo $e->getMessage();
}

// פלט לדוגמה:
// Error: fopen(nonexistentfile.txt): failed to open stream: No such file or directory
// Custom error!
```

## עיון נוסף
בימים הראשונים, שגיאות PHP היו יותר אודות אזהרות והערות שלא הפסיקו את ביצוע הסקריפט. ככל שהשפה התבגרה, היא אימצה טיפול מוכוון עצמים וחזק יותר בשגיאות דרך מחלקת ה-Exception שהוצגה ב-PHP 5. מאוחר יותר, PHP 7 הגיע עם מחלקות Error שבסופו של דבר הבחינו בין שגיאות לחריגות.

לפני הקוביות `try-catch`, PHP השתמשה ב-`set_error_handler()` כדי להתמודד עם שגיאות. `try-catch` היא נקייה ומודרנית יותר. אבל מטפלי שגיאות מותאמים אישית עדיין יש להם מקום, במיוחד עבור קוד ישן או כאשר אתה צריך לתפוס מה שלרוב היו שגיאות שאינן חריגות.

הממשק `Throwable` ב-PHP 7+ אומר שבין אם זה Error או Exception, אתה יכול לתפוס את שניהם. זה שימושי מאוד כי עכשיו אתה לא מפספס שגיאות ריצה קריטיות, שהיו קשות יותר למעקב קודם.

אלטרנטיבות מחוץ למנגנוני הבנויים של PHP כוללות ספריות ופריימוורקים שבאים עם מערכות טיפול בשגיאות משלהם, המציעות עוד יכולות כמו תיעוד שגיאות לקבצים או הצגת דפי שגיאות ידידותיים למשתמש.

## ראה גם
- תיעוד PHP רשמי על חריגות: https://www.php.net/manual/en/language.exceptions.php
- PHP בדרך הנכונה על דיווח שגיאות: https://phptherightway.com/#error_reporting
- מדריך PHP על טיפול בשגיאות: https://www.php.net/manual/en/book.errorfunc.php

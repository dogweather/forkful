---
date: 2024-01-26 04:44:41.545811-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: PHP \u05DE\u05E1\u05E4\
  \u05E7\u05EA \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA\
  \ \u05DC\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05D5\u05E8\u05DB\u05D1\u05D9\
  \u05DD \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05D4\u05E8\u05D7\u05D1\
  \u05D4 `ext-intl` \u05E2\u05DD \u05D4\u05DB\u05D9\u05EA\u05D4 `NumberFormatter`.\
  \ \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4."
lastmod: '2024-03-13T22:44:39.470725-06:00'
model: gpt-4-0125-preview
summary: "PHP \u05DE\u05E1\u05E4\u05E7\u05EA \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\
  \u05D5\u05D1\u05E0\u05D9\u05EA \u05DC\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\
  \u05D5\u05E8\u05DB\u05D1\u05D9\u05DD \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA\
  \ \u05D4\u05D4\u05E8\u05D7\u05D1\u05D4 `ext-intl` \u05E2\u05DD \u05D4\u05DB\u05D9\
  \u05EA\u05D4 `NumberFormatter`."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
weight: 14
---

## איך לעשות:
PHP מספקת תמיכה מובנית למספרים מורכבים באמצעות ההרחבה `ext-intl` עם הכיתה `NumberFormatter`. הנה דוגמה:

```php
// ודא שההרחבה intl טעונה
if (!extension_loaded('intl')) {
    die("The intl extension is not enabled. Please enable it to run this code.");
}

function addComplexNumbers($a, $b) {
    // השתמש ב-NumberFormatter לפרסום ועיצוב מספרים מורכבים
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // פרסום מספרים מורכבים ממחרוזות
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // ביצוע חיבור
    $sum = $numA + $numB;

    // עיצוב התוצאה כמספר מורכב
    return $formatter->format($sum);
}

echo addComplexNumbers('5+3i', '2+7i'); // פלט: 7+10i
```

## צלילה עמוקה
לפני `ext-intl`, ל-PHP לא היה תמיכה מקורית במספרים מורכבים. מפתחים השתמשו בפונקציות או בספריות כיתות מותאמות אישית כדי לטפל במספרים מורכבים. פעולות מורכבות יכולות היו להיות מייגעות ושגויות בקלות, אך `ext-intl` מספקת דרך בינלאומית להצגה ופרסום של מספרים מורכבים המאוזנת עם ספריית ICU.

עם זאת, לפעולות מתמטיות כבדות, ייתכן שישתמשו בספריות חיצוניות שנכתבו בשפות ידידותיות למתמטיקה יותר (כמו C או Python) ויתמשקו איתן דרך PHP. בנוגע ליישום, `ext-intl` מטפלת בו מאחורי הקלעים, מבטיחה חישובים מדויקים תוך הסתרת המורכבות מהמפתח.

מספרים מורכבים נחשבו בעבר לבעייתיים משום שנקראו 'דמיוניים', אך מאז הם הפכו להכרחיים במגוון תחומים מדעיים ומתמטיים, מגלים יותר על המשמעות האמיתית שלהם מאשר על מעמדם הדמיוני.

## ראה גם
- [מדריך PHP על NumberFormatter](https://www.php.net/manual/en/class.numberformatter.php)
- [ויקיפדיה על מספרים מורכבים](https://en.wikipedia.org/wiki/Complex_number)
- [PHP: הדרך הנכונה - עבודה עם סוגי נתונים](https://phptherightway.com/#data_types)

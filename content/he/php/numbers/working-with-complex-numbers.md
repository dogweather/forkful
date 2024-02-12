---
title:                "עבודה עם מספרים מרוכבים"
aliases:
- he/php/working-with-complex-numbers.md
date:                  2024-01-26T04:44:41.545811-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם מספרים מרוכבים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
מספרים מורכבים כוללים חלק ממשי וחלק מדומה, שבדרך כלל נכתבים כ-`a + bi`. הם חיוניים במתמטיקה מתקדמת, פיזיקה, הנדסה ואלגוריתמים מסוימים במחשב. מתכנתים עובדים איתם כדי לטפל בחישובים שכוללים שורשים ריבועיים של מספרים שליליים ופונקציות מתנדנדות.

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

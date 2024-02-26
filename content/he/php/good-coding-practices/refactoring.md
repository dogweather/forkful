---
date: 2024-01-26 01:50:49.863639-07:00
description: "\u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D4\u05D5\
  \u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E9\u05D9\u05E0\u05D5\u05D9\
  \ \u05DE\u05D1\u05E0\u05D4 \u05E9\u05DC \u05E7\u05D5\u05D3 \u05DE\u05D7\u05E9\u05D1\
  \ \u05E7\u05D9\u05D9\u05DD \u05DC\u05DC\u05D0 \u05E9\u05D9\u05E0\u05D5\u05D9 \u05D4\
  \u05EA\u05E0\u05D4\u05D2\u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\
  \u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\
  \u05DD \u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05DB\u05D3\
  \u05D9 \u05DC\u05E9\u05E4\u05E8 \u05D0\u05EA \u05D4\u05EA\u05DB\u05D5\u05E0\u05D5\
  \u05EA \u05D4\u05DC\u05D0 \u05EA\u05E4\u05E7\u05D5\u05D3\u05D9\u05D5\u05EA \u05E9\
  \u05DC \u05D4\u05EA\u05D5\u05DB\u05E0\u05D4,\u2026"
lastmod: '2024-02-25T18:49:37.735174-07:00'
model: gpt-4-0125-preview
summary: "\u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D4\u05D5\
  \u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E9\u05D9\u05E0\u05D5\u05D9\
  \ \u05DE\u05D1\u05E0\u05D4 \u05E9\u05DC \u05E7\u05D5\u05D3 \u05DE\u05D7\u05E9\u05D1\
  \ \u05E7\u05D9\u05D9\u05DD \u05DC\u05DC\u05D0 \u05E9\u05D9\u05E0\u05D5\u05D9 \u05D4\
  \u05EA\u05E0\u05D4\u05D2\u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\
  \u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\
  \u05DD \u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05DB\u05D3\
  \u05D9 \u05DC\u05E9\u05E4\u05E8 \u05D0\u05EA \u05D4\u05EA\u05DB\u05D5\u05E0\u05D5\
  \u05EA \u05D4\u05DC\u05D0 \u05EA\u05E4\u05E7\u05D5\u05D3\u05D9\u05D5\u05EA \u05E9\
  \u05DC \u05D4\u05EA\u05D5\u05DB\u05E0\u05D4,\u2026"
title: "\u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2"
---

{{< edit_this_page >}}

## מה ולמה?
ריפקטורינג הוא תהליך של שינוי מבנה של קוד מחשב קיים ללא שינוי התנהגותו החיצונית. מתכנתים מבצעים ריפקטורינג כדי לשפר את התכונות הלא תפקודיות של התוכנה, ולהפוך את הקוד לנקי יותר, יעיל יותר וקל יותר לתחזוקה.

## איך לבצע:
בואו ניקח קטע קוד PHP קלאסי ונחיל עליו קסם של ריפקטורינג.

לפני הריפקטורינג, הקוד שלנו עשוי להיראות כך:

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Item: " . $item['name'];
        echo " - Price: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Total: " . array_sum(array_column($order, 'price'));
    }
}
```

אך אנו יכולים לרפקטר את הקוד כדי לשפר את בהירותו ומודולריותו:

```php
function printItem($item) {
    echo "Item: {$item['name']} - Price: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Total: " . calculateTotal($order);
    }
}
```
על ידי פיצול הפונקציה `printOrderDetails` לפונקציות קטנות יותר, הקוד שלנו הופך לקריא יותר וקל יותר לנפות באגים.

## צלילה עמוקה
לריפקטורינג יש שורשים בקהילת התכנות של Smalltalk מתחילת שנות ה-90 והתפרסם עוד יותר על ידי הספר המכונן של מרטין פאולר "Refactoring: Improving the Design of Existing Code" (1999). על אף שניתן ליישם ריפקטורינג על כל שפת תכנות, המהות הדינמית של PHP מאפשרת אתגרים והזדמנויות ייחודיים.

חלופות לריפקטורינג עשויות לכלול כתיבה מחדש של קוד, אשר לעיתים קרובות היא סיכונית ודורשת יותר זמן. באקוסיסטם של PHP, כלים כמו PHPStan ו-Rector יכולים לזהות ולבצע אוטומטית פעולות של ריפקטורינג, בהתאמה. מבחינה יישומית, שמירה על ריפקטורינגים קטנים ובדיקה מקיפה באמצעות בדיקות יחידה הם שיטות מפתח להבטיח ריפקטורינג מוצלח ללא הכנסת באגים.

## ראו גם
- ספר הריפקטורינג של מרטין פאולר: https://martinfowler.com/books/refactoring.html
- PHPStan, כלי לניתוח סטטי של PHP: https://phpstan.org/
- Rector, כלי לריפקטורינג אוטומטי של קוד PHP: https://getrector.org/
- בדיקות יחידה ב-PHP עם PHPUnit: https://phpunit.de/

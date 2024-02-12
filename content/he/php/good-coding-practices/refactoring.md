---
title:                "ריפקטורינג"
date:                  2024-01-26T01:50:49.863639-07:00
model:                 gpt-4-0125-preview
simple_title:         "ריפקטורינג"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/refactoring.md"
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

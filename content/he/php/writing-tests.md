---
title:                "כתיבת בדיקות"
date:                  2024-01-19
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"

category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות זה לבדוק שהקוד שלנו עובד כמתוכנן. מתכנתים עושים את זה כדי למצוא ולתקן באגים בצורה מסודרת ולהבטיח איכות ויציבות באפליקציה.

## איך לעשות:
בדוגמה הבאה נשתמש ב-PHPUnit - כלי פופולרי לבדיקות יחידה ב-PHP.

התקנה:
```bash
composer require --dev phpunit/phpunit
```

כתיבת בדיקה בסיסית:
```PHP
<?php
use PHPUnit\Framework\TestCase;

class SampleTest extends TestCase
{
    // בדיקה שאחד ועוד אחד שווים שניים
    public function testAddition()
    {
        $this->assertEquals(2, 1 + 1);
    }
}
```

הרצת הבדיקות:
```bash
./vendor/bin/phpunit tests
```

תוצאת הבדיקה:
```
PHPUnit 9.5.10 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

OK (1 test, 1 assertion)
```

## צלילה עמוקה:
ב-2004 PHPUnit יצא לאור, והשפיע רבות על פיתוח ה-Testing culture ב-PHP. חלופות כוללות Codeception לאינטגרציה ואפשרויות UI, ו-PHPSpec ל-BDD. כאשר אתה כותב בדיקות, חשוב למקם את הבדיקה קרוב לתיקייה של הקוד שהיא בודקת ולהשתמש ב-naming convention שקל להבין.

## גם ראו:
- הדוקומנטציה של PHPUnit: [PHPUnit Manual](https://phpunit.de/manual/current/en/index.html)
- סדרת הדרכות ל-PHP Testing ב-YouTube: [PHP Testing Tutorials](https://www.youtube.com/playlist?list=PLfdtiltiRHWFsPxAGO-SVbCEvhSPHxS7w)
- מאמר על הבדיקות ב-PHP בעבודה מול בסיסי נתונים: [Testing PHP with Databases](https://phpunit.de/manual/current/en/database.html)

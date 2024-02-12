---
title:                "כתיבת בדיקות"
aliases:
- /he/php/writing-tests.md
date:                  2024-02-03T19:31:42.095142-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת בדיקות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות בתכנות כוללת יצירה והרצת סקריפטים שמאמתים שהקוד מתנהג כצפוי תחת תנאים שונים. מתכנתים עושים זאת כדי להבטיח איכות, למנוע נסיגות, ולהקל על שיפוץ בטוח, שהוא קריטי לתחזוקת בסיס קוד בריא, מתפשט וחופשי מבאגים.

## איך לעשות זאת:
### PHP ילידי – PHPUnit
כלי שנמצא בשימוש נרחב לבדיקות ב-PHP הוא PHPUnit. ניתן להתקין אותו באמצעות Composer:
```bash
composer require --dev phpunit/phpunit ^9
```

#### כתיבת בדיקה פשוטה:
צרו קובץ `CalculatorTest.php` בתיקייה `tests`:
```php
use PHPUnit\Framework\TestCase;

// בהנחה שיש לכם מחלקת Calculator שמחברת מספרים
class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```
הריצו את הבדיקות עם:
```bash
./vendor/bin/phpunit tests
```

#### פלט דוגמה:
```
PHPUnit 9.5.10 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

Time: 00:00.005, Memory: 6.00 MB

OK (1 test, 1 assertion)
```

### ספריות של צד שלישי – Mockery
לבדיקות מורכבות, כולל מזיוף אובייקטים, Mockery הוא בחירה פופולרית.

```bash
composer require --dev mockery/mockery
```

#### אינטגרציה של Mockery עם PHPUnit:
```php
use PHPUnit\Framework\TestCase;
use Mockery as m;

class ServiceTest extends TestCase
{
    public function tearDown(): void
    {
        m::close();
    }

    public function testServiceCallsExternalService()
    {
        $externalServiceMock = m::mock(ExternalService::class);
        $externalServiceMock->shouldReceive('process')->once()->andReturn('mocked result');

        $service = new Service($externalServiceMock);
        $result = $service->execute();

        $this->assertEquals('mocked result', $result);
    }
}
```
להרצה, השתמשו באותה פקודת PHPUnit כמו לעיל. Mockery מאפשר אובייקטים דמויים ביטוייתיים וגמישים, מה שמקל על בדיקת אינטראקציות מורכבות בתוך היישום שלכם.

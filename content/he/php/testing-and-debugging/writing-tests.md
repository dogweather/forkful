---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:42.095142-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05DB\u05DC\u05D9 \u05E9\u05E0\u05DE\u05E6\u05D0 \u05D1\u05E9\u05D9\u05DE\u05D5\
  \u05E9 \u05E0\u05E8\u05D7\u05D1 \u05DC\u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D1\
  -PHP \u05D4\u05D5\u05D0 PHPUnit. \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05EA\u05E7\
  \u05D9\u05DF \u05D0\u05D5\u05EA\u05D5 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA\
  \ Composer."
lastmod: '2024-03-13T22:44:39.485910-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05DC\u05D9 \u05E9\u05E0\u05DE\u05E6\u05D0 \u05D1\u05E9\u05D9\u05DE\
  \u05D5\u05E9 \u05E0\u05E8\u05D7\u05D1 \u05DC\u05D1\u05D3\u05D9\u05E7\u05D5\u05EA\
  \ \u05D1-PHP \u05D4\u05D5\u05D0 PHPUnit."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
weight: 36
---

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

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:41:37.250319-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: PHP \u09A4\u09C7 \u099F\u09C7\u09B8\
  \u09CD\u099F\u09BF\u0982\u09AF\u09BC\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\
  \u0995\u099F\u09BF \u09AC\u09CD\u09AF\u09BE\u09AA\u0995\u09AD\u09BE\u09AC\u09C7\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4 \u099F\u09C1\u09B2 \u09B9\u09B2 PHPUnit\u0964\
  \ \u098F\u099F\u09BF \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09C1\u09A8\
  \ \u0995\u09AE\u09CD\u09AA\u09CB\u099C\u09BE\u09B0\u09C7\u09B0 \u09AE\u09BE\u09A7\
  \u09CD\u09AF\u09AE\u09C7."
lastmod: '2024-03-17T18:47:44.134040-06:00'
model: gpt-4-0125-preview
summary: "PHP \u09A4\u09C7 \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982\u09AF\u09BC\u09C7\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09AC\u09CD\u09AF\u09BE\
  \u09AA\u0995\u09AD\u09BE\u09AC\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4\
  \ \u099F\u09C1\u09B2 \u09B9\u09B2 PHPUnit\u0964 \u098F\u099F\u09BF \u0987\u09A8\u09B8\
  \u09CD\u099F\u09B2 \u0995\u09B0\u09C1\u09A8 \u0995\u09AE\u09CD\u09AA\u09CB\u099C\
  \u09BE\u09B0\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কীভাবে:


### নেটিভ PHP – PHPUnit
PHP তে টেস্টিংয়ের জন্য একটি ব্যাপকভাবে ব্যবহৃত টুল হল PHPUnit। এটি ইনস্টল করুন কম্পোজারের মাধ্যমে:
```bash
composer require --dev phpunit/phpunit ^9
```

#### একটি সাধারণ টেস্ট লেখা:
`tests` ডিরেক্টরিতে একটি `CalculatorTest.php` ফাইল তৈরি করুন:
```php
use PHPUnit\Framework\TestCase;

// ধরা যাক, আপনার ক্যালকুলেটর ক্লাস আছে যা সংখ্যা যোগ করে
class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```
টেস্টগুলি চালান এই কমান্ডের মাধ্যমে:
```bash
./vendor/bin/phpunit tests
```

#### নমুনা আউটপুট:
```
PHPUnit 9.5.10 এ দ্বারা সেবাস্টিয়ান বার্গম্যান এবং অবদানকারীরা।

.                                                                   1 / 1 (100%)

সময়: 00:00.005, মেমরি: 6.00 MB

OK (1 টেস্ট, 1 অ্যাসার্শন)
```

### থার্ড-পার্টি লাইব্রেরিস – Mockery
জটিল টেস্টিংয়ের জন্য, যা অবজেক্ট মকিং সহ, Mockery একটি জনপ্রিয় পছন্দ।

```bash
composer require --dev mockery/mockery
```

#### Mockery কে PHPUnit এর সাথে ইন্টিগ্রেট করা:
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
চালানোর জন্য, উপরে দেওয়া PHPUnit কমান্ডটি ব্যবহার করুন। Mockery প্রকাশমুখী এবং নমনীয় মক অবজেক্ট প্রদান করে, যা আপনার অ্যাপ্লিকেশনের মধ্যে জটিল ইন্টারেকশনের টেস্টিং সুবি�

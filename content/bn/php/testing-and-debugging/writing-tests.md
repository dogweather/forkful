---
title:                "টেস্ট লিখা"
date:                  2024-03-17T18:41:37.250319-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কী ও কেন?
প্রোগ্রামিংয়ে টেস্ট লেখা বিভিন্ন শর্তে কোড প্রত্যাশিত আচরণ করে কিনা তা যাচাই করার জন্য স্ক্রিপ্ট তৈরি এবং রান করার প্রক্রিয়াকে বোঝায়। প্রোগ্রামাররা মান নিশ্চিত করতে, প্রত্যাগমন প্রতিরোধ করতে এবং নিরাপদ রিফ্যাক্টরিং করতে এটি করে থাকে যা একটি স্বাস্থ্যকর, স্কেলযোগ্য এবং বাগমুক্ত কোডবেজ বজায় রাখার জন্য অপরিহার্য।

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

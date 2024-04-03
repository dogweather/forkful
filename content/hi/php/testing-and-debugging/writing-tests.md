---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:56.377925-07:00
description: "\u0915\u0948\u0938\u0947: #."
lastmod: '2024-03-13T22:44:52.483990-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\u093E"
weight: 36
---

## कैसे:


### नेटिव PHP – PHPUnit
PHP में परीक्षण के लिए एक व्यापक रूप से प्रयुक्त उपकरण PHPUnit है। इसे Composer के माध्यम से इंस्टॉल करें:
```bash
composer require --dev phpunit/phpunit ^9
```

#### एक साधारण परीक्षण लिखना:
`tests` निर्देशिका में एक `CalculatorTest.php` फाइल बनाएं:
```php
use PHPUnit\Framework\TestCase;

// मान लीजिए कि आपके पास संख्याओं को जोड़ने वाला एक Calculator क्लास है
class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```
परीक्षणों को इस के साथ चलाएँ:
```bash
./vendor/bin/phpunit tests
```

#### नमूना आउटपुट:
```
PHPUnit 9.5.10 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

Time: 00:00.005, Memory: 6.00 MB

OK (1 test, 1 assertion)
```

### तृतीय-पक्ष पुस्तकालय – Mockery
जटिल परीक्षण, जिसमें मॉक ऑब्जेक्ट्स शामिल हैं, के लिए Mockery एक लोकप्रिय विकल्प है।

```bash
composer require --dev mockery/mockery
```

#### PHPUnit के साथ Mockery को एकीकृत करना:
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
चलाने के लिए, ऊपर दिए गए समान PHPUnit कमांड का उपयोग करें। Mockery अभिव्यक्ति और लचीले मॉक ऑब्जेक्ट्स की अनुमति देता है, जिससे आपके अनुप्रयोग के भीतर जटिल आपसी संवादों का परीक्षण करना सुविधाजनक हो जाता है।

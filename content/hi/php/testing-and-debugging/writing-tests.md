---
title:                "टेस्ट लिखना"
aliases: - /hi/php/writing-tests.md
date:                  2024-02-03T19:31:56.377925-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेस्ट लिखना"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
प्रोग्रामिंग में परीक्षण लिखना ऐसी स्क्रिप्ट बनाने और चलाने की प्रक्रिया है जो यह सत्यापित करती हैं कि विभिन्न परिस्थितियों के अन्तर्गत कोड अपेक्षित ढंग से व्यवहार करता है या नहीं। प्रोग्रामर इसे गुणवत्ता सुनिश्चित करने, रिग्रेशंस को रोकने, और सुरक्षित रिफैक्टरिंग की सुविधा देने के लिए करते हैं, जो कि एक स्वस्थ, स्केलेबल, और बग-मुक्त कोडबेस बनाए रखने के लिए महत्वपूर्ण है।

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

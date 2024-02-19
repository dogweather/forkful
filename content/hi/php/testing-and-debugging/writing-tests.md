---
aliases:
- /hi/php/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:56.377925-07:00
description: "\u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917\
  \ \u092E\u0947\u0902 \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u0932\u093F\u0916\
  \u0928\u093E \u0910\u0938\u0940 \u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\
  \u091F \u092C\u0928\u093E\u0928\u0947 \u0914\u0930 \u091A\u0932\u093E\u0928\u0947\
  \ \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\
  \ \u091C\u094B \u092F\u0939 \u0938\u0924\u094D\u092F\u093E\u092A\u093F\u0924 \u0915\
  \u0930\u0924\u0940 \u0939\u0948\u0902 \u0915\u093F \u0935\u093F\u092D\u093F\u0928\
  \u094D\u0928 \u092A\u0930\u093F\u0938\u094D\u0925\u093F\u0924\u093F\u092F\u094B\u0902\
  \ \u0915\u0947 \u0905\u0928\u094D\u0924\u0930\u094D\u0917\u0924 \u0915\u094B\u0921\
  \ \u0905\u092A\u0947\u0915\u094D\u0937\u093F\u0924 \u0922\u0902\u0917\u2026"
lastmod: 2024-02-18 23:09:03.506516
model: gpt-4-0125-preview
summary: "\u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917\
  \ \u092E\u0947\u0902 \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u0932\u093F\u0916\
  \u0928\u093E \u0910\u0938\u0940 \u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\
  \u091F \u092C\u0928\u093E\u0928\u0947 \u0914\u0930 \u091A\u0932\u093E\u0928\u0947\
  \ \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\
  \ \u091C\u094B \u092F\u0939 \u0938\u0924\u094D\u092F\u093E\u092A\u093F\u0924 \u0915\
  \u0930\u0924\u0940 \u0939\u0948\u0902 \u0915\u093F \u0935\u093F\u092D\u093F\u0928\
  \u094D\u0928 \u092A\u0930\u093F\u0938\u094D\u0925\u093F\u0924\u093F\u092F\u094B\u0902\
  \ \u0915\u0947 \u0905\u0928\u094D\u0924\u0930\u094D\u0917\u0924 \u0915\u094B\u0921\
  \ \u0905\u092A\u0947\u0915\u094D\u0937\u093F\u0924 \u0922\u0902\u0917\u2026"
title: "\u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\u093E"
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

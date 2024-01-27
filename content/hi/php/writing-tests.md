---
title:                "परीक्षण लिखना"
date:                  2024-01-19
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (क्या & क्यों?)
टेस्ट लेखन यानी कोड की जांच करने के लिए खास टेस्ट केस तैयार करना। यह प्रोग्रामर्स को बग्स खोजने, सॉफ्टवेयर की क्वालिटी सुधारने और नई फीचर्स को बिना डरे जोड़ने में मदद करता है।

## How to: (कैसे करें:)
आइए PHPUnit का उपयोग करके बेसिक टेस्ट केस लिखना सीखें। सबसे पहले, `composer` के जरिए PHPUnit install करें।

```PHP
composer require --dev phpunit/phpunit
```

अब एक सिंपल PHP क्लास बनाएं जो किसी संख्या को दोगुना कर देगी:

```PHP
class Doubler {
    public function double($number) {
        return $number * 2;
    }
}
```

फिर इसके लिए एक टेस्ट केस लिखें:

```PHP
use PHPUnit\Framework\TestCase;

class DoublerTest extends TestCase {
    public function testDouble() {
        $doubler = new Doubler();
        $this->assertEquals(4, $doubler->double(2));
    }
}
```

जब आप `phpunit` चलाएंगे, आपको कुछ ऐसा आउटपुट दिखाई देगा:

```
OK (1 test, 1 assertion)
```

## Deep Dive (गहराई में जानकारी)
PHPUnit की शुरुआत 2004 में Sebastian Bergmann ने की थी, और आज यह PHP का मानक यूनिट टेस्टिंग फ्रेमवर्क है। टेस्ट ड्रिवन डेवलपमेंट (TDD) और बिहेवियर ड्रिवन डेवलपमेंट (BDD) जैसी अल्टरनेटिव टेस्टिंग प्रैक्टिसेज भी हैं। PHPUnit के साथ मॉक ऑब्जेक्ट्स, डेटा प्रोवाइडर्स और डिपेंडेंसी इंजेक्शन जैसी उन्नत तकनीकों का इस्तेमाल भी होता है।

## See Also (इसे भी देखें)
- [PHPUnit Manual](https://phpunit.de/manual/current/en/index.html)
- [Composer](https://getcomposer.org/)
- [PHP: The Right Way](https://phptherightway.com/#testing)
- [Martin Fowler's guide to Continuous Integration](https://martinfowler.com/articles/continuousIntegration.html)

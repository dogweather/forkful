---
date: 2024-01-26 04:26:17.441615-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0938\u092C\u0938\
  \u0947 \u092A\u0939\u0932\u0947, \u0938\u0941\u0928\u093F\u0936\u094D\u091A\u093F\
  \u0924 \u0915\u0930\u0947\u0902 \u0915\u093F \u0906\u092A\u0915\u0947 \u092A\u093E\
  \u0938 \u0915\u094B\u0908 TOML \u092A\u093E\u0930\u094D\u0938\u0930 \u0932\u093E\
  \u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0907\u0902\u0938\u094D\u091F\u0949\u0932\
  \ \u0939\u094B, \u091C\u0948\u0938\u0947 \u0915\u093F `yosymfony/toml`. \u091A\u0932\
  \u093F\u090F \u090F\u0915 TOML \u092B\u093E\u0907\u0932 \u0915\u094B \u092A\u093E\
  \u0930\u094D\u0938 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902."
lastmod: '2024-03-13T22:44:52.518649-06:00'
model: gpt-4-0125-preview
summary: "\u0938\u092C\u0938\u0947 \u092A\u0939\u0932\u0947, \u0938\u0941\u0928\u093F\
  \u0936\u094D\u091A\u093F\u0924 \u0915\u0930\u0947\u0902 \u0915\u093F \u0906\u092A\
  \u0915\u0947 \u092A\u093E\u0938 \u0915\u094B\u0908 TOML \u092A\u093E\u0930\u094D\
  \u0938\u0930 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0907\u0902\
  \u0938\u094D\u091F\u0949\u0932 \u0939\u094B, \u091C\u0948\u0938\u0947 \u0915\u093F\
  \ `yosymfony/toml`."
title: "TOML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 39
---

## कैसे करें:
सबसे पहले, सुनिश्चित करें कि आपके पास कोई TOML पार्सर लाइब्रेरी इंस्टॉल हो, जैसे कि `yosymfony/toml`. चलिए एक TOML फाइल को पार्स करते हैं:

```php
composer require yosymfony/toml

<?php
require 'vendor/autoload.php';

use Yosymfony\Toml\Toml;

$tomlString = <<<TOML
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
TOML;

$array = Toml::Parse($tomlString);

print_r($array);
```

नमूना आउटपुट:

```
Array
(
    [database] => Array
        (
            [server] => 192.168.1.1
            [ports] => Array
                (
                    [0] => 8001
                    [1] => 8001
                    [2] => 8002
                )

            [connection_max] => 5000
            [enabled] => 1
        )

)
```

## गहरी समझ
TOML 2013 में आया था, GitHub के सह-संस्थापक टॉम प्रेस्टन-वर्नर द्वारा कॉन्फिग फाइलों के लिए XML और JSON के अधिक उपयोगकर्ता-मित्रवत विकल्प के रूप में तैयार किया गया। जहां JSON मशीनों के लिए साधारण है, TOML की संरचना इसे मनुष्यों की आंखों के लिए आसान बनाती है, बिना YAML की जटिलता के।

TOML के विकल्पों में JSON, YAML, और XML शामिल हैं। प्रत्येक की अपनी ताकत और अनुप्रयोग संदर्भ हैं। JSON सर्वव्यापी और भाषा-स्वतंत्र है; YAML अधिक पठनीय है और टिप्पणियों का समर्थन करता है, जबकि XML विस्तृत और व्यापक रूप से समर्थित है।

PHP में TOML लागू करते समय, आप उन लाइब्रेरीज की तलाश में हैं जो इसकी सामग्री को PHP एरेज़ या वस्तुओं में पार्स करती हैं। `yosymfony/toml` TOML विनिर्देश v0.4.0 का अनुसरण करने वाला एक PHP पार्सर है। नवीनतम के साथ बने रहने के लिए, हमेशा नए पार्सर्स या अपडेट्स की तलाश करें जो सबसे वर्तमान TOML संस्करण (v1.0.0 जैसा कि मेरे अंतिम अपडेट के समय) का समर्थन करते हैं।

## देखें भी
- TOML विनिर्देश: <https://toml.io/>
- PHP के लिए TOML पार्सर (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- डेटा प्रारूपों की तुलना (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- PHP पैकेज मैनेजर (कंपोजर): <https://getcomposer.org/>

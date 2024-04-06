---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:52.368535-07:00
description: "\u0915\u0948\u0938\u0947: PHP, \u0905\u092A\u0928\u0947 \u0935\u0930\
  \u094D\u0924\u092E\u093E\u0928 \u0938\u0902\u0938\u094D\u0915\u0930\u0923\u094B\u0902\
  \ \u092E\u0947\u0902, \u0921\u093F\u092B\u093C\u0949\u0932\u094D\u091F \u0932\u093E\
  \u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u0947 \u092D\u093E\u0917 \u0915\
  \u0947 \u0930\u0942\u092A \u092E\u0947\u0902 YAML \u092A\u093E\u0930\u094D\u0938\
  \u093F\u0902\u0917 \u0915\u093E \u0938\u092E\u0930\u094D\u0925\u0928 \u0928\u0939\
  \u0940\u0902 \u0915\u0930\u0924\u0940 \u0939\u0948\u0964 PHP \u092E\u0947\u0902\
  \ YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u0947\
  \ \u0915\u093E \u0938\u092C\u0938\u0947\u2026"
lastmod: '2024-03-13T22:44:52.513690-06:00'
model: gpt-4-0125-preview
summary: "PHP, \u0905\u092A\u0928\u0947 \u0935\u0930\u094D\u0924\u092E\u093E\u0928\
  \ \u0938\u0902\u0938\u094D\u0915\u0930\u0923\u094B\u0902 \u092E\u0947\u0902, \u0921\
  \u093F\u092B\u093C\u0949\u0932\u094D\u091F \u0932\u093E\u0907\u092C\u094D\u0930\u0947\
  \u0930\u0940 \u0915\u0947 \u092D\u093E\u0917 \u0915\u0947 \u0930\u0942\u092A \u092E\
  \u0947\u0902 YAML \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u0915\u093E\
  \ \u0938\u092E\u0930\u094D\u0925\u0928 \u0928\u0939\u0940\u0902 \u0915\u0930\u0924\
  \u0940 \u0939\u0948\u0964 PHP \u092E\u0947\u0902 YAML \u0915\u0947 \u0938\u093E\u0925\
  \ \u0915\u093E\u092E \u0915\u0930\u0928\u0947 \u0915\u093E \u0938\u092C\u0938\u0947\
  \ \u0938\u0940\u0927\u093E \u0924\u0930\u0940\u0915\u093E Symfony YAML \u0915\u094C\
  \u092E\u094D\u092A\u094B\u0928\u0945\u0928\u094D\u091F \u092F\u093E `yaml` PECL\
  \ \u090F\u0915\u094D\u0938\u091F\u0947\u0902\u0936\u0928 \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930\u0928\u093E \u0939\u0948\u0964\n\n\u0938\u092C\u0938\
  \u0947 \u092A\u0939\u0932\u0947, \u0915\u0902\u092A\u094B\u091C\u093C\u0930 \u0915\
  \u0947 \u092E\u093E\u0927\u094D\u092F\u092E \u0938\u0947 Symfony YAML \u0915\u094C\
  \u092E\u094D\u092A\u094B\u0928\u0945\u0928\u094D\u091F \u0938\u094D\u0925\u093E\u092A\
  \u093F\u0924 \u0915\u0930\u0947\u0902."
title: "YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 41
---

## कैसे:
PHP, अपने वर्तमान संस्करणों में, डिफ़ॉल्ट लाइब्रेरी के भाग के रूप में YAML पार्सिंग का समर्थन नहीं करती है। PHP में YAML के साथ काम करने का सबसे सीधा तरीका Symfony YAML कौम्पोनॅन्ट या `yaml` PECL एक्सटेंशन का उपयोग करना है।

### Symfony YAML कौम्पोनॅन्ट का उपयोग करना
सबसे पहले, कंपोज़र के माध्यम से Symfony YAML कौम्पोनॅन्ट स्थापित करें:

```bash
composer require symfony/yaml
```

फिर, आप YAML सामग्री को पार्स और डंप कर सकते हैं निम्नानुसार:

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// YAML पार्सिंग
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// एक ऐरे से YAML बनाना
$array = [
    'greet' => 'Hello, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = Yaml::dump($array);
echo $yaml;
```

पार्सिंग के समय नमूना आउटपुट:

```
Array
(
    [greet] => Hello, World!
    [framework] => Array
        (
            [name] => Symfony
            [language] => PHP
        )

)
```

डंप करते समय नमूना आउटपुट:

```
greet: Hello, YAML!
framework:
    name: Symfony
    language: PHP
```

### `yaml` PECL एक्सटेंशन का उपयोग करना
आप चाहें तो, या यदि आपकी परियोजना आवश्यकताएं अनुमति देती हैं, PECL एक्सटेंशन YAML के साथ काम करने का एक और कुशल तरीका हो सकता है। सबसे पहले, सुनिश्चित करें कि एक्सटेंशन स्थापित है:

```bash
pecl install yaml
```

फिर, अपनी `php.ini` कॉन्फ़िगरेशन में इसे सक्षम करें:

```ini
extension=yaml.so
```

YAML को पार्स करने और उत्सर्जन करने का तरीका यह है:

```php
<?php

// YAML पार्सिंग
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// एक ऐरे से YAML बनाना
$array = [
    'greet' => 'Hello, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = yaml_emit($array);
echo $yaml;
```

आउटपुट Symfony कौम्पोनॅन्ट के समान होगा, यह दर्शाते हुए कि YAML मानव-पठनीय प्रारूप और PHP ऐरे संरचनाओं के बीच एक पुल की भूमिका निभाता है, आसान कॉन्फ़िगरेशन और डेटा हैंडलिंग को सुविधाजनक बनाता है।

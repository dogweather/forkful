---
title:                "YAML के साथ काम करना"
date:                  2024-02-03T19:26:52.368535-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

YAML, जिसका पूरा नाम "YAML Ain't Markup Language" है, एक मानव-पठनीय डेटा सिरियलाइज़ेशन प्रारूप है जो आमतौर पर कॉन्फ़िगरेशन फाइलों के लिए प्रयुक्त होता है। प्रोग्रामर्स YAML का उपयोग इसकी सादगी और पठनीयता के कारण करते हैं, जिससे यह सेटिंग्स, पैरामीटर्स और यहां तक कि जटिल डेटा संरचनाओं को आसानी से प्रबंधनीय फॉर्म में संग्रहीत करने के लिए एक उत्कृष्ट विकल्प बन जाता है।

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

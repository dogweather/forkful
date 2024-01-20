---
title:                "यामल के साथ काम करना"
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
YAML, जिसका पूरा नाम "YAML Ain't Markup Language" है, एक डाटा सीरियलाइजेशन फॉर्मेट है। PHP में इसका उपयोग कॉन्फ़िगुरेशन फाइल्स, डाटा स्टोरेज, और ऐप्लीकेशंस के बीच डाटा एक्सचेंज के लिए किया जाता है क्योंकि यह मानव और मशीन, दोनों के लिए पढ़ने में आसान है।

## How to: (कैसे करें:)
PHP में YAML के साथ काम करने के लिए `yaml` एक्सटेंशन की जरूरत होती है, जिसे पहले इंस्टॉल करना पड़ता है।

**YAML को PHP एर्रे में पार्स करना:**
```PHP
<?php
$yaml = <<<EOD
name: John Doe
age: 30
skills:
  - PHP
  - JavaScript
  - HTML
EOD;

$array = yaml_parse($yaml);

print_r($array);
```
आउटपुट:
```
Array
(
    [name] => John Doe
    [age] => 30
    [skills] => Array
        (
            [0] => PHP
            [1] => JavaScript
            [2] => HTML
        )

)
```

**PHP एर्रे को YAML स्ट्रिंग में कन्वर्ट करना:**
```PHP
<?php
$array = [
    'name' => 'John Doe',
    'age' => 30,
    'skills' => ['PHP', 'JavaScript', 'HTML'],
];

$yaml = yaml_emit($array);

echo $yaml;
```
आउटपुट:
```
name: John Doe
age: 30
skills:
  - PHP
  - JavaScript
  - HTML
```

## Deep Dive (गहराई से समझिए)
YAML, 2001 में विकसित हुआ था। यह JSON और XML जैसे अन्य सीरियलाइजेशन फॉर्मेट्स का एक सरल विकल्प है। PHP में `yaml_parse()` और `yaml_emit()` फंक्शन्स YAML कंटेंट को पार्स और जनरेट करने के लिए होते हैं। ये कार्यक्षमता PECL लाइब्रेरी के `yaml` एक्सटेंशन के माध्यम से उपलब्ध होती है।

## See Also (इसे भी देखें)
- [YAML वेबसाइट](https://yaml.org/)
- [PHP.net पर yaml फंक्शन्स](https://www.php.net/manual/en/book.yaml.php)
- [PECL YAML एक्सटेंशन](https://pecl.php.net/package/yaml)
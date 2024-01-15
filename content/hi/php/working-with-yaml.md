---
title:                "yaml के साथ काम करना"
html_title:           "PHP: yaml के साथ काम करना"
simple_title:         "yaml के साथ काम करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

*YAML* काफी सरल एवं कंपैक्ट एक plaintext स्ट्रक्चर बनाने के लिए उत्कृष्ट उपकरण हो सकता है। इसलिए, इसका उपयोग विभिन्न ऑनलाइन ऐप्लिकेशन और वेबसाइट्स पर दस्तावेज का स्टोरेज, कॉन्फ़िगरेशन फ़ाइल्स के रूप में आदि किया जा सकता है। 

## कैसे इसका उपयोग करें

यदि आप PHP में YAML का उपयोग शुरू करना चाहते हैं, तो आपको आरंभिक रूप से `yaml` पैकेज को इनस्टॉल करना होगा। आप निम्नलिखित कमांड के साथ इसे इनस्टॉल कर सकते हैं:

```PHP
composer require symfony/yaml
```

इस कमांड का उपयोग करके हम स्ट्रिंग का एक यामल फ़ॉर्मेट में पार्स कर सकते हैं:

```PHP
use Symfony\Component\Yaml\Yaml;

$yaml = "name: John\nage: 30\noccupation: Developer";
$array = Yaml::parse($yaml);
```

यहां, `$array` एक ऐसा एरे होगा जिसमें हमारी यामल स्ट्रिंग के कुंजियां और मान शामिल होंगे। इसका आउटपुट निम्नलिखित हो सकता है:

```PHP
array(3) {
  ["name"]=>
  string(4) "John"
  ["age"]=>
  int(30)
  ["occupation"]=>
  string(9) "Developer"
}
```

इसके अलावा, हम यामल फ़ाइल्स को लोड करने और सेव करने के लिए भी `parseFile()` और `dumpFile()` फ़ंक्शन का उपयोग कर सकते हैं। आप निम्नलिखित कोड ब्लॉक देख सकते हैं:

```PHP
// फाइल से पार्स करें
$array = Yaml::parseFile('config.yml');

// फाइल में सेव करें
$yaml = Yaml::dumpFile('config.yml', $array);
```

## गहराई में जाएं

यामल में कई प्रत्येकों हैं, जिनका आप उपयोग कर सकते हैं। यदि आपको अधिक जानकारी चाहिए, तो आप आध
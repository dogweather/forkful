---
title:                "PHP: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/working-with-json.md"
---

{{< edit_this_page >}}

## बताते हैं की क्यों करें हम JSON के साथ काम?

JSON एक आसान, लोकप्रिय और लोगों के बीच पसंद किया जाने वाला डेटा का संरचना प्रारूप है। हम JSON का उपयोग अपने वेब एप्लिकेशन में कर सकते हैं ताकि हम डेटा को आसानी से प्रसंस्करण और ट्रांसफर कर सकें। 

## कैसे करें JSON के साथ काम?

```PHP
// डेटा बनाएं
$data = array(
  'name' => 'रवि',
  'age' => 25,
  'city' => 'मुंबई'
);

// JSON में बदलें
$json = json_encode($data);

// समाचार को कंसोल पर प्रिंट करें
echo $json;
```

आउटपुट:
```PHP
{"name":"रवि","age":25,"city":"मुंबई"}
```

## डीप डाइव: JSON के साथ कैसे काम करें?

- JSON को अपनी वेब एप्लिकेशन में असामान्य से अधिक डेटा तक पहुंचया जा सकता है।
- एकरूपता प्राप्त करने के लिए, हम डेटा को सामान्य रूप से ऑप्शन या प्रोपर्टी के रूप में या फिर कमजोरी या डिलीट की शर्तों से हटकर भी स्वरूपित कर सकते हैं।
- JSON को अन्य डेटा संरचना प्रारूपों में रूपांतरित करने के लिए कई उपयोगी फंक्शन हैं।

## देखें भी: 

- JSON क्या है और उसे कैसे उपयोग करें: https://www.php.net/manual/en/function.json-encode.php
- अन्य डेटा संरचनाएं: https://www.php.net/manual/en/function.json-decode.php
- हॉटेल की बुकिंग फॉर्म का उदाहरण: https://www.php.net/manual/en/function.json-last-error.php
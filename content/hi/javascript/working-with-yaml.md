---
title:                "यामल के साथ काम करना"
html_title:           "Javascript: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
क्या आपने कभी YAML के बारे में सुना है? यह एक फॉर्मेट है जो डेटा को संरचित करने और भंडारित करने के लिए उपयोग किया जाता है। यह एक पाठ का आसान और साफ तरीका है जो डेटा पारगमन और डेटा विनिस्त उचित रूप से संरचित करता है। YAML यूएमईएल पहले से ही गोल्ड स्टैंडर्ड है और वैभवपूर्ण समर्थन को प्रदान करता है।

## कैसे करें:
```Javascript
// शंखनाद फ़ंक्शन से एक यूएमईएल फ़ाइल को पढ़ें
const fs = require('fs');
const yaml = require('js-yaml');

try {
  const data = yaml.safeLoad(fs.readFileSync('फ़ाइल.यूएमईएल', 'utf8'));
  console.log(data);
} catch (e) {
  console.log(e);
}

// जावास्क्रिप्ट ऑब्जेक्ट को यूएमईएल फ़ॉर्मेट में रूपांतरित करें
const data = { name: 'शुरुआत', उम्र: 10 };
const yamlData = yaml.dump(data);
console.log(yamlData);
```

आउटपुट:
```Javascript
{
  "name": "शुरुआत",
  "उम्र": 10
}
"name: शुरुआत\nउम्र: 10\n"
```

## गहराई में दिखावट:
YAML को 2001 में घोषित किया गया था और इसकी मूल विभागीय संरचना Perl के CPAN दस्तावेज़ के आधार पर बनाई गई थी। YAML के कुछ प्रमुख विकल्प शामिल हैं - JSON, XML और CSV जैसे प्रारूपों को भी डेटा को संरचित करने के लिए इस्तेमाल किया जा सकता है। यूएमईएल को प्राथमिक रूप से दोगुने नोड विश्लेषण के रूप में कॉन्सुम किया जाता है और इसे YAML का रूपांतरण नहीं कहा जाता है।

## भी देखें:
- YAML की आधिकारिक साइट (https://yaml.org/)
- YAML के JavaScript में अनुप्रयोग (https://github.com/nodeca/js-yaml)
- YAML और JSON के बीच की तुलना (https://www.smashingmagazine.com/2016/07/yaml-primer-part-1/)
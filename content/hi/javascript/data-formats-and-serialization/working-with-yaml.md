---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:26.079113-07:00
description: "YAML, \u091C\u093F\u0938\u0947 YAML Ain't Markup Language \u0915\u0947\
  \ \u0930\u0942\u092A \u092E\u0947\u0902 \u091C\u093E\u0928\u093E \u091C\u093E\u0924\
  \u093E \u0939\u0948, \u090F\u0915 \u092E\u093E\u0928\u0935-\u092A\u0920\u0928\u0940\
  \u092F \u0921\u0947\u091F\u093E \u0938\u0940\u0930\u093F\u092F\u0932\u093E\u0907\
  \u091C\u0947\u0936\u0928 \u092A\u094D\u0930\u093E\u0930\u0942\u092A \u0939\u0948\
  \u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0905\u0915\
  \u094D\u0938\u0930 \u0907\u0938\u0947 \u0935\u093F\u0928\u094D\u092F\u093E\u0938\
  \ \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0914\u0930 \u092D\u093E\u0937\u093E\
  \u0913\u0902\u2026"
lastmod: 2024-02-19 22:05:12.061835
model: gpt-4-0125-preview
summary: "YAML, \u091C\u093F\u0938\u0947 YAML Ain't Markup Language \u0915\u0947 \u0930\
  \u0942\u092A \u092E\u0947\u0902 \u091C\u093E\u0928\u093E \u091C\u093E\u0924\u093E\
  \ \u0939\u0948, \u090F\u0915 \u092E\u093E\u0928\u0935-\u092A\u0920\u0928\u0940\u092F\
  \ \u0921\u0947\u091F\u093E \u0938\u0940\u0930\u093F\u092F\u0932\u093E\u0907\u091C\
  \u0947\u0936\u0928 \u092A\u094D\u0930\u093E\u0930\u0942\u092A \u0939\u0948\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0905\u0915\u094D\
  \u0938\u0930 \u0907\u0938\u0947 \u0935\u093F\u0928\u094D\u092F\u093E\u0938 \u092B\
  \u093C\u093E\u0907\u0932\u094B\u0902 \u0914\u0930 \u092D\u093E\u0937\u093E\u0913\
  \u0902\u2026"
title: "YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

YAML, जिसे YAML Ain't Markup Language के रूप में जाना जाता है, एक मानव-पठनीय डेटा सीरियलाइजेशन प्रारूप है। प्रोग्रामर अक्सर इसे विन्यास फ़ाइलों और भाषाओं के बीच डेटा आदान-प्रदान के लिए उपयोग करते हैं क्योंकि इसकी सरलता और पठनीयता JSON या XML की तुलना में अधिक होती है।

## कैसे करें:

JavaScript में, YAML के साथ काम करना आमतौर पर तीसरे पक्ष की लाइब्रेरी का उपयोग करने में शामिल होता है क्योंकि भाषा में YAML के लिए एक निर्मित पार्सर शामिल नहीं है। इस उद्देश्य के लिए सबसे लोकप्रिय लाइब्रेरियों में से एक `js-yaml` है। आप `js-yaml` का उपयोग करके YAML को JavaScript ऑब्जेक्ट्स में पार्स कर सकते हैं और इसके विपरीत।

सबसे पहले, आपको `js-yaml` स्थापित करने की आवश्यकता है:

```bash
npm install js-yaml
```

फिर, आप इसे अपने प्रोजेक्ट्स में उपयोग कर सकते हैं। यहाँ आप कैसे एक YAML फ़ाइल को लोड कर सकते हैं और इसे एक JavaScript ऑब्जेक्ट में पार्स कर सकते हैं:

```javascript
// जेस-यैमल मॉड्यूल को आवश्यकता है
const yaml = require('js-yaml');
const fs   = require('fs');

// एक फ़ाइल से YAML लोड करें
try {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

यदि आपकी `config.yaml` फ़ाइल इस तरह दिखती है:

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

तो आउटपुट होगा:

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

इसके विपरीत करने के लिए, एक JavaScript ऑब्जेक्ट को YAML स्ट्रिंग में परिवर्तित करना:

```javascript
const yaml = require('js-yaml');
const obj = {
  version: 1,
  services: {
    web: {
      image: "myapp/web:latest",
      ports: ["5000:5000"]
    }
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

इस कोड से निम्न प्राप्त होगा:

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

`js-yaml` का उपयोग करके, आप अपने JavaScript प्रोजेक्ट्स में YAML पार्सिंग और सीरियलाइजेशन को आसानी से एकीकृत कर सकते हैं, डेटा इंटरएक्सचेंजेबिलिटी और विन्यास प्रबंधन को बढ़ावा देते हैं।

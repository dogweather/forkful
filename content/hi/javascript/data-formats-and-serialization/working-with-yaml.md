---
title:                "YAML के साथ काम करना"
date:                  2024-02-03T19:26:26.079113-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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

---
title:                "TOML के साथ काम करना"
date:                  2024-01-26T04:24:39.646212-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML, जिसकी लम्बी अवधारणा टॉम की स्पष्ट, न्यूनतम भाषा है, यह परिभाषित करता है कि कॉन्फ़िग फ़ाइलों को कैसे संरचित किया जाए। प्रोग्रामर TOML के साथ काम करते हैं क्योंकि इसे पढ़ना, लिखना आसान होता है, और यह एक हैश टेबल के लिए अच्छी तरह से मैप करता है, जिससे यह कॉन्फ़िगरेशन के लिए एक प्राथमिक विकल्प बन जाता है।

## कैसे:
JavaScript में TOML के साथ काम करने के लिए, आपको एक पार्सर की आवश्यकता होगी जैसे कि `@iarna/toml`। पहले, इसे स्थापित करें: `npm install @iarna/toml`। फिर, एक TOML स्ट्रिंग को एक JavaScript ऑब्जेक्ट में पार्स करें या एक JavaScript ऑब्जेक्ट को TOML फॉर्मेट में स्ट्रिंगिफ़ाई करें।

```javascript
const toml = require('@iarna/toml');

// TOML स्ट्रिंग को JS ऑब्जेक्ट में पार्स करें
const tomlStr = `
title = "TOML उदाहरण"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// JS ऑब्जेक्ट को TOML स्ट्रिंग में बदलें
const jsObject = {
  title: "TOML उदाहरण",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## गहराई से जानकारी
TOML को पहली बार 2013 में टॉम प्रेस्टन-वर्नर द्वारा रिलीज़ किया गया था, जो GitHub के सह-संस्थापक हैं। इसे INI जैसे अन्य प्रारूपों को प्रतिस्थापित करने के लिए डिज़ाइन किया गया था, क्योंकि यह अधिक मानकीकृत और पार्स करने के लिए आसान होता है। JSON और YAML विकल्प हैं, लेकिन वे बहुत जटिल या बहुत लचीले हो सकते हैं। TOML का लाभ स्थैतिक कॉन्फ़िगरेशन में होता है जहां एक सरल, स्पष्ट प्रारूप पसंद किया जाता है। इसका डिजाइन एक हैश टेबल में सरल मैपिंग की अनुमति देता है, जिसमें कुंजी और मान संपत्ति नामों और उनके मानों से मेल खाते हैं। व्यापक उपयोग के लिए, आपको TOML और अन्य प्रारूपों के बीच कनवर्ट करने में सक्षम उपकरणों को एकीकृत करने की आवश्यकता हो सकती है क्योंकि विभिन्न इकोसिस्टम समर्थन में भिन्नता होती है।

## यह भी देखें
- आधिकारिक TOML GitHub रिपॉजिटरी: https://github.com/toml-lang/toml
- TOML बनाम YAML बनाम JSON तुलना: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- npm `@iarna/toml` पैकेज: https://www.npmjs.com/package/@iarna/toml

---
title:                "मानक त्रुटि के लिए लिखना"
aliases: - /hi/javascript/writing-to-standard-error.md
date:                  2024-02-03T19:34:36.069168-07:00
model:                 gpt-4-0125-preview
simple_title:         "मानक त्रुटि के लिए लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
जावास्क्रिप्ट में स्टैंडर्ड एरर (stderr) पर लिखना यह है कि त्रुटि संदेशों या किसी आलोचनात्मक सूचना को एक विशिष्ट, अलग स्ट्रीम में निर्देशित करना, जो विशेष रूप से Unix जैसे वातावरणों में लॉगिंग और डीबगिंग उद्देश्यों के लिए उपयोगी होता है। प्रोग्रामर इसे त्रुटि संदेशों से सामान्य प्रोग्राम आउटपुट को अलग करने के लिए करते हैं, जिससे आउटपुट प्रबंधन स्पष्ट और त्रुटि की निगरानी आसान होती है।

## कैसे:
Node.js में, stderr पर लिखना `console.error()` मेथड का उपयोग करके या सीधे `process.stderr` पर लिखकर पूरा किया जा सकता है। यहाँ दोनों दृष्टिकोणों का उदाहरण दिखाया गया है:

```javascript
// console.error() का उपयोग करते हुए
console.error('यह एक त्रुटि संदेश है।');

// सीधे process.stderr पर लिखना
process.stderr.write('यह एक और त्रुटि संदेश है।\n');
```

दोनों विधियों के लिए नमूना आउटपुट stderr स्ट्रीम में प्रकट होगा, stdout के साथ मिलान नहीं करेगा:
```
यह एक त्रुटि संदेश है।
यह एक और त्रुटि संदेश है।
```

अधिक सुव्यवस्थित या एप्लिकेशन-विशिष्ट लॉगिंग के लिए, बहुत से JavaScript प्रोग्रामर `winston` या `bunyan` जैसी थर्ड-पार्टी लाइब्रेरियों का उपयोग करते हैं। यहाँ `winston` का उपयोग करते हुए एक त्वरित उदाहरण है:

पहले, npm के माध्यम से `winston` को इंस्टाल करें:
```shell
npm install winston
```

फिर, stderr पर त्रुटियों को लॉग करने के लिए `winston` को कॉन्फ़िगर करें:
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// एक त्रुटि संदेश लॉग करना
logger.error('winston के माध्यम से लॉग की गई त्रुटि।');
```

यह सेटअप सुनिश्चित करता है कि जब आप `winston` का उपयोग करके एक त्रुटि को लॉग करते हैं, तो यह stderr में निर्देशित होती है, जिससे सामान्य और त्रुटि आउटपुट्स के बीच स्पष्ट विभाजन बना रहता है।

---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 
प्रिंटिंग डिबग आउटपुट से हमारा मतलब होता है कोड के रन होने के दौरान ऑपरेशनल डिटेल्स को कंसोल या फाइल में सेव करना. यह प्रोग्रामर्स को कोड की दुर्विधा से निपटने के लिए मदद करता है, क्योंकि इससे हमें सिस्टम की क्रियावाईयों का एक स्पष्ट विचार मिलता है।

## कैसे करें:
```Javascript
// 'console.log' का उपयोग डिबग आउटपुट को प्रिंट करने के लिए
let a = 5;
let b = 10;
console.log('Adding', a, '+', b); // "Adding 5 + 10"
let sum = a + b;
console.log('Result:', sum); // "Result: 15"
```
इसमें हमने ‘console.log’ का उपयोग किया है, जो ब्राउज़र के कंसोल पर मैसेज प्रिंट करता है।

## गहराई बात:
1. **ऐतिहासिक प्रसंग:** 'console.log' फ़ंक्शन का उपयोग डिबगिंग के लिए किसी भी JavaScript एन्वायरन्मेंट में किया जा सकता है। इसकी बड़ी वजह यह है कि इसे JavaScript के पहले वर्जन में ही जोड़ा गया था।
2. **विकल्प:** डिबगिंग के लिए 'debugger' स्टेटमेंट और ब्राउज़र डेवलपर टूल्स का भी उपयोग किया जा सकता है।
3. **कार्यान्वयन विवरण:** 'console.log' प्रिंट करता है कंसोल पर, जिसमें यह डिबग खोजने में सहायता करता है। यह हमें आउटपुट को नोट करने की अनुमति देता है।

## देखें भी:
1. Mozilla Developer Network (MDN) - Console API: https://developer.mozilla.org/en-US/docs/Web/API/Console
2. Chrome DevTools - JavaScript Debugging: https://developers.google.com/web/tools/chrome-devtools/javascript
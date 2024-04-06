---
date: 2024-01-20 17:35:30.592263-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): \u092A\u094D\
  \u0930\u093E\u0930\u0902\u092D \u092E\u0947\u0902, \u091C\u093E\u0935\u093E\u0938\
  \u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F \u092E\u0947\u0902 \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B `+` \u0913\u092A\u0930\
  \u0947\u091F\u0930 \u0938\u0947 \u091C\u094B\u095C\u093E \u091C\u093E\u0924\u093E\
  \ \u0925\u093E\u0964 ES6 \u0905\u092A\u0921\u0947\u091F \u0915\u0947 \u0938\u093E\
  \u0925, `template literals` \u0928\u0947 \u0907\u0938\u0947 \u0914\u0930 \u0906\u0938\
  \u093E\u0928 \u092C\u0928\u093E \u0926\u093F\u092F\u093E\u2026"
lastmod: '2024-04-05T22:51:07.636979-06:00'
model: gpt-4-1106-preview
summary: "\u092A\u094D\u0930\u093E\u0930\u0902\u092D \u092E\u0947\u0902, \u091C\u093E\
  \u0935\u093E\u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F \u092E\u0947\u0902\
  \ \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B `+`\
  \ \u0913\u092A\u0930\u0947\u091F\u0930 \u0938\u0947 \u091C\u094B\u095C\u093E \u091C\
  \u093E\u0924\u093E \u0925\u093E\u0964 ES6 \u0905\u092A\u0921\u0947\u091F \u0915\u0947\
  \ \u0938\u093E\u0925, `template literals` \u0928\u0947 \u0907\u0938\u0947 \u0914\
  \u0930 \u0906\u0938\u093E\u0928 \u092C\u0928\u093E \u0926\u093F\u092F\u093E \u0939\
  \u0948, \u091C\u093F\u0938\u0938\u0947 \u0915\u0940 \u0935\u0948\u0930\u093F\u090F\
  \u092C\u0932\u094D\u0938 \u0914\u0930 \u090F\u0915\u094D\u0938\u092A\u094D\u0930\
  \u0948\u0936\u0928 \u0915\u094B \u0938\u0940\u0927\u0947 \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917\u094D\u0938 \u092E\u0947\u0902 \u0907\u0902\u092C\u0947\
  \u0921 \u0915\u093F\u092F\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\
  \u0964 \u092E\u0932\u094D\u091F\u0940-\u0932\u093E\u0907\u0928 \u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0914\u0930 \u0907\u0902\u091F\u0930\
  \u092A\u094B\u0932\u0947\u0936\u0928 \u0907\u0938\u094D\u092E\u0947\u0902 \u0938\
  \u0940\u0927\u0947 \u0939\u094B \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u0932\u092E\u094D\u092C\u0947 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \u094D\u0938 \u0915\u094B \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0939\u0948\
  \u0902\u0921\u0932 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0914\
  \u0930 \u0930\u0928\u091F\u093E\u0907\u092E \u092A\u0930 \u0921\u093E\u092F\u0928\
  \u093E\u092E\u093F\u0915 \u0930\u0942\u092A \u0938\u0947 \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917\u094D\u0938 \u091C\u094B\u095C\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F, \u092F\u0939 \u092C\u0939\u0941\u0924 \u0939\u0940 \u0909\u092A\
  \u092F\u094B\u0917\u0940 \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u0911\u0932\
  \u094D\u091F\u0930\u0928\u0947\u091F\u093F\u0935 \u0915\u0947 \u0930\u0942\u092A\
  \ \u092E\u0947\u0902, `Array.join()` \u0914\u0930 `concat()` \u092E\u0947\u0925\u0921\
  \ \u092D\u0940 \u092E\u094C\u091C\u0942\u0926 \u0939\u0948\u0902, \u0932\u0947\u0915\
  \u093F\u0928 \u0906\u092E\u0924\u094C\u0930 \u092A\u0930 template literals \u0915\
  \u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u091C\u094D\u092F\u093E\
  \u0926\u093E \u092C\u0947\u0939\u0924\u0930 \u092E\u093E\u0928\u093E \u091C\u093E\
  \u0924\u093E \u0939\u0948\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091C\u094B\
  \u0921\u093C\u0928\u093E"
weight: 3
---

## How to (कैसे करें):
```Javascript
// Plus operator (+) का इस्तेमाल करके स्ट्रिंग्स जोड़ना
let greeting = "नमस्ते";
let message = "दुनिया";
let combinedMessage = greeting + " " + message;
console.log(combinedMessage); // "नमस्ते दुनिया"

// Template literals (backticks ` `) का इस्तेमाल करके स्ट्रिंग्स जोड़ना
let user = "रोहन";
let action = "कोडिंग";
console.log(`${user} अभी ${action} कर रहा है।`); // "रोहन अभी कोडिंग कर रहा है।"
```

## Deep Dive (गहराई से जानकारी):
प्रारंभ में, जावास्क्रिप्ट में स्ट्रिंग्स को `+` ओपरेटर से जोड़ा जाता था। ES6 अपडेट के साथ, `template literals` ने इसे और आसान बना दिया है, जिससे की वैरिएबल्स और एक्सप्रैशन को सीधे स्ट्रिंग्स में इंबेड किया जा सकता है। मल्टी-लाइन स्ट्रिंग्स और इंटरपोलेशन इस्में सीधे हो सकते हैं। लम्बे स्ट्रिंग्स को आसानी से हैंडल करने के लिए और रनटाइम पर डायनामिक रूप से स्ट्रिंग्स जोड़ने के लिए, यह बहुत ही उपयोगी होता है। ऑल्टरनेटिव के रूप में, `Array.join()` और `concat()` मेथड भी मौजूद हैं, लेकिन आमतौर पर template literals का इस्तेमाल ज्यादा बेहतर माना जाता है।

## See Also (और भी देखें):
- MDN Web Docs on String concatenation: [String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- Template literals (Template strings) on MDN: [Template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- JavaScript ES6 features overview: [ES6 Features](https://exploringjs.com/es6.html)

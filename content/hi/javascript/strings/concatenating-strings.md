---
date: 2024-01-20 17:35:30.592263-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): ."
lastmod: '2024-03-13T22:44:52.976027-06:00'
model: gpt-4-1106-preview
summary: .
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

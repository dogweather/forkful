---
aliases:
- /hi/javascript/concatenating-strings/
date: 2024-01-20 17:35:30.592263-07:00
description: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u093E\
  \ \u091C\u094B\u0921\u093C\u0928\u093E \u092E\u0924\u0932\u092C \u0926\u094B \u092F\
  \u093E \u0909\u0938\u0938\u0947 \u091C\u094D\u092F\u093E\u0926\u093E \u091F\u0947\
  \u0915\u094D\u0938\u094D\u091F \u092A\u0940\u0938\u0947\u0938 \u0915\u094B \u0905\
  \u0915\u091F\u094D\u0920\u093E \u0915\u0930\u0928\u093E\u0964 \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0907\
  \u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u094D\
  \u092F\u094B\u0902\u0915\u093F \u0905\u0915\u094D\u0938\u0930 \u0921\u093E\u091F\
  \u093E \u0915\u094B \u0909\u0938\u0930-\u092B\u094D\u0930\u0947\u0902\u0921\u0932\
  \u0940 \u0924\u0930\u0940\u0915\u0947 \u0938\u0947\u2026"
lastmod: 2024-02-18 23:09:04.029165
model: gpt-4-1106-preview
summary: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u093E\
  \ \u091C\u094B\u0921\u093C\u0928\u093E \u092E\u0924\u0932\u092C \u0926\u094B \u092F\
  \u093E \u0909\u0938\u0938\u0947 \u091C\u094D\u092F\u093E\u0926\u093E \u091F\u0947\
  \u0915\u094D\u0938\u094D\u091F \u092A\u0940\u0938\u0947\u0938 \u0915\u094B \u0905\
  \u0915\u091F\u094D\u0920\u093E \u0915\u0930\u0928\u093E\u0964 \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0907\
  \u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u094D\
  \u092F\u094B\u0902\u0915\u093F \u0905\u0915\u094D\u0938\u0930 \u0921\u093E\u091F\
  \u093E \u0915\u094B \u0909\u0938\u0930-\u092B\u094D\u0930\u0947\u0902\u0921\u0932\
  \u0940 \u0924\u0930\u0940\u0915\u0947 \u0938\u0947\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091C\u094B\
  \u0921\u093C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग्स का जोड़ना मतलब दो या उससे ज्यादा टेक्स्ट पीसेस को अकट्ठा करना। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि अक्सर डाटा को उसर-फ्रेंडली तरीके से प्रेजेंट करने के लिए जोड़ना पड़ता है।

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

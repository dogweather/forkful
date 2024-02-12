---
title:                "स्ट्रिंग को जोड़ना"
aliases:
- /hi/javascript/concatenating-strings.md
date:                  2024-01-20T17:35:30.592263-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/concatenating-strings.md"
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

---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "Javascript: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कोई व्यक्ति आइटम को आदेश दालने, या उसको सुधारने की ख्वाहिश रख सकता है, जैसे कि टाइपिंग में शब्दों को पढ़ने को आसान बनाने के लिए। यहां हम स्ट्रिंग को capitalize करना सीखेंगे।

## कैसे करें

```Javascript
let string = "hello world";

// Using built-in methods
let capitalizedString = string.charAt(0).toUpperCase() + string.slice(1);

console.log(capitalizedString); // Output: Hello world

// Using ES6 spread operator and template literals
let spreadString = [...string];
spreadString[0] = spreadString[0].toUpperCase();
let capitalizedString = spreadString.join("");

console.log(capitalizedString); // Output: Hello world
```
## गहराई में जाएं

जिन strings के शुरू में स्ट्रिंग को capitalize करने के लिए हम वांछित नतीजा प्राप्त करना चाहते हैं, एक संभवतः स्ट्रिंग क्षेत्र में प्रवेश स्थल की जरूरत होती है। उदाहरण के लिए, यदि हम "javascript is awesome" capitalize करने की कोशिश कर रहे हैं, तो हमारे पास एक स्पेस स्थान होगा जहां हम स्ट्रिंग को capitalize करना चाहते हैं। इस स्पेस को हम स्पेस से बदल सकते हैं, उदाहरण के लिए एक हाइफन या अपवर्तित टिल्ड। इसके अलावा, हम ES6 के साथ स्थिति अधिकारी जैसे कि ``|`` उपयोग कर सकते हैं। इन विभिन्न मेथड्स का उपयोग करते हुए, हम स्ट्रिंग को सीधी और अधिक अंदाज़ी से capitalize कर सकते हैं।

## भी देखें

- [String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase) - MDN डॉक्यूमेंटेशन में आधिकारिक रूप से समर्थित स्ट्रिंग अपरकेसिंग।
- [ES6 spread operator](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax) - अतिरिक्त स्प्रेड ऑपरेटरों का उपयोग करके स्ट्रिंग को अलग-अलग इलेमेंट्स में विभाजित किया
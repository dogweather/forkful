---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग की लंबाई खोजना मतलब उसमें कितने अक्षर हैं, यह जानना। प्रोग्रामर इसे करते हैं क्योंकि कभी-कभी संवादाता या इनपुट स्ट्रिंग के आकार पर निर्भर करता है।

## कैसे: 

आइए एक TypeScript कोड से इसके उदाहरण देखते हैं:

```TypeScript
let name: string = "Deepak Sharma";
console.log(name.length);
```

ऊपरी कोड चलाने पर आपको "Deepak Sharma" के अक्षरों की संख्या मिलेगी, जो 13 है। इस प्रकार, आप किसी भी स्ट्रिंग की लंबाई खोज सकते हैं।

## गहराई में:

1. हिस्टोरिकल कन्टेक्स्ट: JavaScript और इसके स्वरूपों (जैसे कि TypeScript) में, `length` प्रॉपर्टी का उपयोग किसी स्ट्रिंग की लंबाई प्राप्त करने के लिए किया जाता है। यह सबसे सामान्य रूप से इस्तेमाल किया जाने वाला तरीका है। 
2. वैकल्पिक विधियाँ: `length` के अलावा, आप इंटरेस्टिंग लिब्रेरीज (जैसे lodash) का उपयोग करके भी स्ट्रिंग की लंबाई खोज सकते हैं, लेकिन `length` की तरह सीधे और सरल तरीके से।
3. आयामन विवरण: आपके स्ट्रिंग में Unicode वर्ण हो सकते हैं, जिनके सामान्य `length` आपकी उम्मीद के बराबर नहीं हो सकता। इससे निपटने के लिए, आप `.length` से बेहतर "Grapheme Splitter" लाइब्रेस जैसे उपकरणों का उपयोग कर सकते हैं।

## देखिए भी:

1. [JavaScript string length property | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
2. [Understanding JavaScript's `length`](https://v8.dev/blog/strings#but-first-the-length)

TypeScript में स्ट्रिंग की लंबाई खोजना आसान है, परन्तु विविध परिस्थितियों में दाक्षिण्य एंड ध्यान देने की आवश्यकता है। जन्नती गहराई में जानेवालों के लिए, अपने ज्ञान को बढाने के लिए ऊपर दिए गए लिंक्स पर जाएं।
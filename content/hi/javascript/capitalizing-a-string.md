---
title:                "स्ट्रिंग को कैपिटलाइज करना"
html_title:           "Javascript: स्ट्रिंग को कैपिटलाइज करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## व्हाट एंड व्हाय? (What & Why?)

एक स्ट्रिंग को कैपिटलाइज़ करना, उसे शुरुआत में बड़े अक्षर(Session capital letter) में बदलने को कहते हैं। प्रोग्रामर इसे पाठ को अधिक पठनीय और सुंदर बनाने के लिए करते हैं।

## हाउ टू (How to)

यहां हम देखेंगे कि कैसे हम String पर `toUpperCase()` और `charAt()` जावास्क्रिप्ट के मेथड का उपयोग करके कैपिटलाइज कर सकते हैं। 

```Javascript
function capitalizeFirstChar(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}
console.log(capitalizeFirstChar('hello world')); 
```

ऊपर बताए गए कोड का आउटपुट - 'Hello world' होगा।

## डीप डाइव (Deep Dive)

- **ऐतिहासिक प्रसंग (Historical Context)**: जावास्क्रिप्ट में String.toUpperCase() और String.charAt() मेथड्स 1st वर्जन से ही मौजूद थे।

- **विकल्प (Alternatives)**: आप या तो `String.prototype` का उपयोग करके अपना कस्टम मेथड बना सकते हैं, या किसी पुस्तकालय (जैसे lodash) का उपयोग कर सकते हैं। 

- **आंतरिक विवरण (Implementation Details)**: `String.charAt()` मेथड स्ट्रिंग में निर्दिष्ट स्थान पर वर्ण वापस करता है। `String.toUpperCase()` मेथड एक नई स्ट्रिंग वापस करता है, जिसमें शेष स्ट्रिंग के सभी अक्षर कैपिटल लेटर्स में होते हैं।

## सी अल्सो(See Also)

अतिरिक्त जानकारी के लिए, निम्नलिखित लिंक पर जा सकते हैं:

1. [MDN String.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
2. [MDN String.charAt()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
3. [ECMAScript Specification](https://tc39.es/ecma262/)
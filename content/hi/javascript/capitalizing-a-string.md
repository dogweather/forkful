---
title:                "Javascript: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

जावास्क्रिप्ट में स्ट्रिंग को कैपिटलाइज करने से हमें इस स्ट्रिंग को उपयोग करने में आसानी होगी और यह प्रोग्रामिंग कोड को पढ़ने में भी सुविधाजनक होगा।

## कैसे करें

```Javascript
let str = "hello world";
str = str.toUpperCase();
console.log(str); // Output: HELLO WORLD
```

## गहराई में जाएं

स्ट्रिंग को कैपिटलाइज करने के लिए हम `.toUpperCase()` फंक्शन का उपयोग कर सकते हैं। यह फंक्शन दिया हुआ स्ट्रिंग को अपर केस में रिटर्न करता है। स्ट्रिंग को कैपिटलाइज करने का यह तरीका सबसे सरल और आसान है।

## और भी देखें

अधिक जानकारी के लिए देखें: 
- [String.prototype.toUpperCase() documentation from MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Capitalizing a string in JavaScript from GeeksforGeeks](https://www.geeksforgeeks.org/capitalize-first-letter-of-a-string-in-javascript/)
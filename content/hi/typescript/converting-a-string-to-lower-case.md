---
title:                "TypeScript: स्ट्रिंग को लोअर केस में रूपांतरण करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरण करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें एक स्ट्रिंग को लोअर केस में कनवर्ट करने की ज़रूरत होती है, जैसे की उपयोगकर्ता से लेन-देन के समय या इस्टीमेटिंग में। इससे स्ट्रिंग को समझने और साथ ही प्रोग्रामिंग लैंग्वेज में काम करने में भी आसानी होती है।

## कैसे करें

```TypeScript
let str = "HELLO WORLD";
console.log(str.toLowerCase());
```

Output: "hello world"

यहां हमने "HELLO WORLD" नामक स्ट्रिंग को लोअर केस में कनवर्ट किया है और उसका आउटपुट प्रिंट किया है। हम इसके लिए TypeScript के inbuilt `toLowerCase()` फ़ंक्शन का इस्तेमाल किा है। इससे स्ट्रिंग के हर एक अक्षर को लोअर केस में बदल जाता है।

## गहराई से जानें

स्ट्रिंग को लोअर केस में कनवर्ट करने के लिए कई तरीके हो सकते हैं। यहां हमने TypeScipt के inbuilt फंक्शन का उपयोग किया है, लेकिन अन्य प्रोग्रामिंग लैंग्वेज में भी ऐसे फंक्शन मौजूद होते हैं। इसके अलावा हम `for` लूप या `map` फंक्शन के साथ भी कई तरीकों से स्ट्रिंग को लोअर केस में कन्वर्ट कर सकते हैं।

## देखें

यदि आप और भी TypeScipt सीखना चाहते हैं तो निम्न लिंकों को ज़रूर देखें:

- https://www.typescriptlang.org/
- https://www.typescriptlang.org/docs/home.html
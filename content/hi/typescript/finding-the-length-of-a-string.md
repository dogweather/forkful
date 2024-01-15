---
title:                "स्ट्रिंग की लंबाई का पता लगाना"
html_title:           "TypeScript: स्ट्रिंग की लंबाई का पता लगाना"
simple_title:         "स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें किसी भी प्रोग्रामिंग भाषा में अपने कोड की आवश्यकता होती है ताकि हम डेटा की लंबाई को जान सकें। इसी तरह, TypeScript में भी स्ट्रिंग की लंबाई जानने के लिए एक विशेष तरीका है।

## कैसे करें

```TypeScript
let string = "Hello World";

console.log(string.length);
// आउटपुट: 11
```

यहां हमने `string` नाम की एक चर बनाई और उसकी लंबाई को `length` फंक्शन की मदद से प्रिंट किया। हम इस तरह से `.` नोटेशन का भी उपयोग कर सकते हैं:

```TypeScript
console.log("Hello World".length);
// आउटपुट: 11
```

## गहराई में जाएँ

जब हम `length` फंक्शन को इस्तेमाल करते हैं, तो वह दरअसल `string` के `length` प्रोपर्टी को रिटर्न करती है। अगर हम इसके साथ `.` नोटेशन का उपयोग न करें तो हम `TypeError` देखेंगे।

इससे बाहर, हम `length` को स्ट्रिंग के सभी चरों को जोड़ने के लिए इस्तेमाल भी कर सकते हैं, जैसे:

```TypeScript
console.log("Hello".length + "World".length);
// आउटपुट: 10
```

## देखें भी

- [JavaScript Strings](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/home.html)
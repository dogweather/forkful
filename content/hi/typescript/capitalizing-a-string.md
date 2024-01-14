---
title:    "TypeScript: एक स्ट्रिंग कैपिटलाइज़ करना"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

व्यक्तियों को स्ट्रिंग को कैपिटलाइज़ करने में रुचि हो सकती है, जो उनके प्रोग्रामिंग उपकरणों को अधिक पढ़ने और समझने में मदद कर सकता है। इससे उनके प्रोग्राम को रोबोस्ट और आसान बनाने में मदद मिलती है।

## कैसे करें

```
टाइपस्क्रिप्ट द्वारा स्ट्रिंग कैपिटलाइज़ करने के लिए आप निम्नलिखित कोड उपयोग कर सकते हैं।

const str = "hello, world!";
const capitalizedStr = str.toUpperCase();

console.log(capitalizedStr); // output: HELLO, WORLD!

```

इस कोड में, हमने स्ट्रिंग वेरिएबल को बनाया है और उसे `toUpperCase()` फ़ंक्शन से लेकर उसे कैपिटलाइज़ किया है और फिर हमने कॉन्सोल में उसका आउटपुट प्रिंट किया है। इस तरह हम स्ट्रिंग को कैपिटलाइज़ कर सकते हैं।

## गहराई में जाएं

स्ट्रिंग को कैपिटलाइज़ करने के लिए अन्य तरीके भी मौजूद हैं, जैसे अपने कार्यों को सुधारने के लिए सामान्य लूपिंग रिक्त्सा का उपयोग करना। अधिक गहराई से जानने के लिए, आप निम्नलिखित लिंकों पर जा सकते हैं:

- [JavaScript स्ट्रिंग प्रोटोटाइप कैपिटलाइज़िशन मेथड](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [क्यों और कैसे करें: स्ट्रिंग को कैपिटलाइज़ करना](https://www.digitalocean.com/community/tutorials/how-and-why-to-capitalize-strings-in-javascript)
- [TypeScript स्ट्रिंग मैनिपुलेशन के लिए स्पेशल टाइप्स और फ़ंक्शन समूह](https://pawelgrzybek.com/typescript-string-manipulation-types-and-functions/) 

## देखें भी

- अन्य टाइपस्क्रिप्ट प्रोग्रामिंग
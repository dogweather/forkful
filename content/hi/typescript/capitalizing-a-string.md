---
title:                "स्ट्रिंग को कैपिटलाइज करना"
html_title:           "TypeScript: स्ट्रिंग को कैपिटलाइज करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग को कैपिटलाइज करने से हमारे मोड्यूल, चर, या किसी भी वाक्यांश के पहले अक्षर को बड़ा अक्षर (Uppercase) में बदला जा सकता है। कार्यक्रमकर्ता इसे चीजों की पहचान को बेहतर और पठनीयता को बढ़ाने के लिए करते हैं।

## कैसे करें:

```TypeScript
function capitalize(s: string): string {
  return s.charAt(0).toUpperCase() + s.slice(1);
}

console.log(capitalize("typescript"));
```

Output:

```
TypeScript
```

## गहरी चर्चा

अक्सर capitalization का उपयोग वाक्यों, शीर्षकों और उपयोगकर्ता नामों को प्रमुख बनाने के लिए किया जाता है। इसका लोगों में यह धारना पैदा करने का इतिहासिक प्रयोग होता है कि बड़े अक्षर की उपेक्षा की जा सकती है।
JavaScript method `.toUpperCase()` या Lodash जैसी बाहरी लाइब्रेरियों का इस्तेमाल करने का वैकल्पिक तरीका भी है, लेकिन TypeScript में हमें स्थायी टाइप सुरक्षा मिलती है। 
यह सुनिश्चित करता है कि केवल स्ट्रिंग्स को ही capitalize किया जा सकता है, और उन्हें सही ढंग से handling किया जाता है।

## देखें भी

- TypeScript डॉक्युमेंटेशन: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- MDN Web Docs (JavaScript .toUpperCase()): [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- Lodash Library: [https://lodash.com/](https://lodash.com/)
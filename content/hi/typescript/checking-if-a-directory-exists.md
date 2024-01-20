---
title:                "डायरेक्टरी का अस्तित्व जाँचना"
date:                  2024-01-20T14:59:14.586916-07:00
html_title:           "Elm: डायरेक्टरी का अस्तित्व जाँचना"
simple_title:         "डायरेक्टरी का अस्तित्व जाँचना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

डायरेक्टरी की जांच करने का मतलब है कि हम कंप्यूटर में एक खास फोल्डर के अस्तित्व की पुष्टि करते हैं। प्रोग्रामर्स इसका उपयोग फाइल सिस्टम में विशेष डेटा के पढ़ने, लिखने या मॉडिफिकेशन से पहले सत्यापन के लिए करते हैं।

## How to: (कैसे करें:)

TypeScript में `fs` मॉड्यूल का `accessSync` मेथड या `existsSync` मेथड का प्रयोग करके हम जांच सकते हैं कि डायरेक्टरी मौजूद है या नहीं।

```typescript
import { accessSync, constants } from 'fs';

try {
  // डायरेक्टरी का पथ दिया गया है
  accessSync('/path/to/directory', constants.F_OK);
  
  console.log('डायरेक्टरी मौजूद है।');
} catch (error) {
  console.error('डायरेक्टरी मौजूद नहीं है।');
}
```

```typescript
import { existsSync } from 'fs';

// डायरेक्टरी का पथ दिया गया है
if (existsSync('/path/to/directory')) {
  console.log('डायरेक्टरी मौजूद है।');
} else {
  console.error('डायरेक्टरी मौजूद नहीं है।');
}
```

## Deep Dive (गहन अन्वेषण):

Node.js `fs` मॉड्यूल पुराना है और फाइल सिस्टम ऑपरेशंस के साथ शुरुआती दिनों से ही उपयोग में है। `existsSync` मेथड का उपयोग सुविधाजनक होता है, लेकिन यह `deprecate` हो चुका है क्योंकि यह Boolean वापस करता है जो error handling को सीमित करता है। इसकी जगह `accessSync` मेथड का प्रयोग करने की सिफारिश की जाती है, जो कि errors throw करता है और ज्यादा robust error handling की अनुमति देता है।

हालांकि, ये मेथड्स synchronous हैं, और इनका प्रयोग ब्लॉकिंग हो सकता है। इसलिए बड़े एप्लिकेशन में जहाँ परफॉर्मेंस महत्वपूर्ण है, वहाँ एसिंक्रोनस वेरिएंट्स `access` और `exists` (जिन्हें प्रॉमिसेज या कॉलबैक पैटर्न के साथ प्रयोग किया जा सकता है) का उपयोग करना बेहतर होगा।

## See Also (देखें भी):

- Node.js Documentation for the `fs` module: [Node.js fs Module](https://nodejs.org/api/fs.html)
- TypeScript Documentation: [TypeScript Lang](https://www.typescriptlang.org/docs/)
- MDN Web Docs on asynchronous programming: [Asynchronous Programming](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous)
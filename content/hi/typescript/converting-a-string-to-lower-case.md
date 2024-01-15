---
title:                "स्ट्रिंग को लोअर केस में रूपांतरित करना"
html_title:           "TypeScript: स्ट्रिंग को लोअर केस में रूपांतरित करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरित करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

कोई इससे लाभान्वित हो सकता है क्योंकि lower case में रूपांतरण किया जाना स्ट्रिंग को जोड़ने द्वारा, पठनीयता बढ़ाई जा सकती है और प्रोग्राम को अधिक पढ़ने और समझने में सुविधा प्रदान की जा सकती है।

## कैसे करें

कोड उदाहरण:

 ```TypeScript 
 let inputString: string = "Hello World";
 let outputString: string = inputString.toLowerCase();

 console.log(outputString);
 ```
आउटपुट:

```
hello world
```
## डीप डाइव

lower case में रूपांतरण करने के नियम हैं जो स्ट्रिंग के प्रत्येक चरित्र को निर्दिष्ट छोटे चरित्र में परिवर्तित करता है। यह प्रक्रिया शून्य स्थान, संख्यात्मक चरित्र और अन्य अनपेक्षित चरित्रों को स्वचालित रूप से नज़रअंदाज करती है। lower case में रूपांतरण करने के लिए, TypeScript स्ट्रिंग के लिए विशिष्ट फ़ंक्शन `toLowerCase()` प्रदान करता है। 

## देखें

इस लेख के अलावा आप और भी अधिक जानकारी प्राप्त कर सकते हैं:

- [TypeScript डॉक्यूमेंटेशन](https://www.typescriptlang.org/docs/)
- [MDN lower case में रूपांतरण](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
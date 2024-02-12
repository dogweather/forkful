---
title:                "संख्याओं को पूर्णांक बनाना"
aliases: - /hi/typescript/rounding-numbers.md
date:                  2024-01-26T03:48:06.852016-07:00
model:                 gpt-4-0125-preview
simple_title:         "संख्याओं को पूर्णांक बनाना"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/rounding-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
संख्याओं को गोल करना एक विशिष्ट सटीकता तक संख्या को छाँटने का काम है। प्रोग्रामर इसे न्यूमेरिकल आउटपुट को पठनीयता, प्रदर्शनीयता या जब किसी विशिष्ट सटीकता की आवश्यकता होती है तो इसे नियंत्रित करने के लिए करते हैं, खासकर जब ऑपरेशन परिणामस्वरूप फ्लोटिंग-पॉइंट प्राप्त होते हैं।

## कैसे:
TypeScript में संख्याओं को गोल करना कई तरीकों से किया जा सकता है। यहाँ एक त्वरित अवलोकन दिया गया है:

```typescript
// Math.round सबसे निकटतम पूर्णांक तक गोल करता है
console.log(Math.round(1.5)); // आउटपुट: 2

// Math.ceil सबसे निकटतम पूर्णांक तक ऊपर की ओर गोल करता है
console.log(Math.ceil(1.1)); // आउटपुट: 2

// Math.floor सबसे निकटतम पूर्णांक तक नीचे की ओर गोल करता है
console.log(Math.floor(1.8)); // आउटपुट: 1

// toFixed एक निश्चित संख्या में दशमलव स्थानों तक गोल करता है
let num = 1.23456;
console.log(num.toFixed(2)); // आउटपुट: "1.23"
// ध्यान दें: toFixed एक स्ट्रिंग वापस करता है! आवश्यकता होने पर वापस परिवर्तित करने के लिए parseFloat का उपयोग करें।
console.log(parseFloat(num.toFixed(2))); // आउटपुट: 1.23

```

## गहराई में
पहले के दिनों में, सीमित स्थान और प्रिसिजन मुद्दों के कारण संख्याओं को गोल करना आवश्यक था। आज, फ्लोटिंग-पॉइंट अंकगणित बाइनरी में संख्याओं को कैसे संग्रहीत किया जाता है, इसके कारण अजीब परिणामों को प्राप्त कर सकता है। गोल करने के विकल्प में फ्लोर, सील, और ट्रंक (बिना गोल किए दशमलव को हटाने के लिए) शामिल हैं।

आंतरिक महत्व हैं: `Math.round` "राउंड हाफ अप" (जिसे "कमर्शियल राउंडिंग" भी कहा जाता है) का अनुसरण करता है, जबकि `Math.floor` और `Math.ceil` सीधे होते हैं। `toFixed` अप्रत्याशित परिणाम पैदा कर सकता है क्योंकि यह एक स्ट्रिंग लौटाता है, और यह "राउंड हाफ टू ईवन" (जिसे "बैंकर्स राउंडिंग" भी कहा जाता है) का उपयोग करता है, विशेषकर एक ही संख्याओं को कई बार गोल करने में पूर्वाग्रह को कम करने के लिए उपयोगी है।

## देखें
- [MDN - Math.round()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [IEEE मानक फ्लोटिंग-पॉइंट अंकगणित के लिए (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)

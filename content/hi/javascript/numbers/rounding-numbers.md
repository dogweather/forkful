---
date: 2024-01-26 03:46:26.325656-07:00
description: "\u0915\u0948\u0938\u0947: \u092F\u0939\u093E\u0901 \u0906\u092A `Math.round()`,\
  \ `Math.ceil()`, \u0914\u0930 `Math.floor()` \u0915\u093E \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930\u0915\u0947 JavaScript \u092E\u0947\u0902 \u0938\u0902\u0916\
  \u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u0915\u0948\u0938\u0947 \u0930\u093E\
  \u0909\u0902\u0921 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902."
lastmod: '2024-03-13T22:44:52.981107-06:00'
model: gpt-4-0125-preview
summary: "\u092F\u0939\u093E\u0901 \u0906\u092A `Math.round()`, `Math.ceil()`, \u0914\
  \u0930 `Math.floor()` \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 JavaScript \u092E\u0947\u0902 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902\
  \ \u0915\u094B \u0915\u0948\u0938\u0947 \u0930\u093E\u0909\u0902\u0921 \u0915\u0930\
  \ \u0938\u0915\u0924\u0947 \u0939\u0948\u0902."
title: "\u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u092A\u0942\
  \u0930\u094D\u0923\u093E\u0902\u0915 \u092C\u0928\u093E\u0928\u093E"
weight: 13
---

## कैसे:
यहाँ आप `Math.round()`, `Math.ceil()`, और `Math.floor()` का उपयोग करके JavaScript में संख्याओं को कैसे राउंड कर सकते हैं:

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (क्योंकि .567 है .5 से अधिक)

console.log(roundedDown); // प्रिंट करता है: 2
console.log(roundedUp);   // प्रिंट करता है: 3
console.log(rounded);     // प्रिंट करता है: 3
```

एक निश्चित संख्या की दशमलव जगहों पर सेट करने के लिए, `toFixed()` का उपयोग करें:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (एक स्ट्रिंग लौटाता है)

console.log(twoDecimals); // प्रिंट करता है: "2.57"
```

स्ट्रिंग को वापस एक संख्या में बदलने के लिए एक यूनेरी प्लस या `Number()` का उपयोग करें:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // प्रिंट करता है: 2.57
```

## गहराई में
संख्याओं का राउंडिंग नया नहीं है; यह संख्याओं के जितना पुराना है। JavaScript में, `Math.round()` "राउंड हाफ अप" टाई-ब्रेकिंग का उपयोग करता है: यदि फ्रैक्शनल भाग 0.5 है, तो यह निकटतम सम नंबर पर राउंड करता है।

अधिक नियंत्रण के लिए, `toFixed()` आपकी जाने-माने विधि हो सकती है, लेकिन याद रखें, यह एक स्ट्रिंग लौटाता है। वापस एक संख्या में परिवर्तन एक अतिरिक्त कदम हो सकता है लेकिन यह सुनिश्चित करता है कि आप न्यूमेरिक प्रकारों के साथ काम करते रहें।

विकल्प? `lodash` जैसी पुस्तकालय `_.round(number, [precision=0])` की पेशकश करती है जो अधिक सूक्ष्म नियंत्रण के लिए है। या, नवीन `Intl.NumberFormat` आपको केवल राउंडिंग से परे उच्च-प्रेसिजन फॉर्मेटिंग प्रदान करता है।

प्रेसिजन की बात करें तो, JavaScript में फ्लोटिंग-पॉइंट की विचित्रताओं के बारे में सावधान रहें। `0.1 + 0.2` बिल्कुल `0.3` के बराबर नहीं होता है, जिस तरह से संख्याएं संग्रहित होती हैं। कभी-कभी, ऐसी फ्लोटिंग-पॉइंट त्रुटियों को सही करने के लिए राउंडिंग आवश्यक हो जाती है।

## देखें भी
- मोजिला का मैथ डाॅक्यूमेंटेशन: [MDN वेब डॉक्स](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- `Intl.NumberFormat` के साथ वित्तीय राउंडिंग: [ECMAScript इंटरनेशनलाइज़ेशन API](https://tc39.es/ecma402/#numberformat-objects)
- `lodash` राउंडिंग: [Lodash डॉक्स](https://lodash.com/docs/4.17.15#round)

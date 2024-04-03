---
date: 2024-01-20 17:37:28.476045-07:00
description: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\
  \u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u090F\u0915 \u0924\u093E\u0930\u0940\
  \u0916 \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F \u0915\u094B \u092A\u0922\
  \u093C\u0928\u0947 \u0932\u093E\u092F\u0915 \u091F\u0947\u0915\u094D\u0938\u094D\
  \u091F \u092B\u0949\u0930\u094D\u092E\u0947\u091F \u092E\u0947\u0902 \u092A\u0930\
  \u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E\u0964 \u0921\
  \u0947\u0935\u0932\u092A\u0930\u094D\u0938 \u0907\u0938\u0947 \u092F\u0942\u091C\
  \u093C\u0930 \u0907\u0902\u091F\u0930\u092B\u0947\u0938 \u092E\u0947\u0902 \u0924\
  \u093E\u0930\u0940\u0916 \u0926\u093F\u0916\u093E\u0928\u0947,\u2026"
lastmod: '2024-03-13T22:44:53.009329-06:00'
model: gpt-4-1106-preview
summary: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\u093E\
  \ \u092E\u0924\u0932\u092C \u0939\u0948 \u090F\u0915 \u0924\u093E\u0930\u0940\u0916\
  \ \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F \u0915\u094B \u092A\u0922\u093C\
  \u0928\u0947 \u0932\u093E\u092F\u0915 \u091F\u0947\u0915\u094D\u0938\u094D\u091F\
  \ \u092B\u0949\u0930\u094D\u092E\u0947\u091F \u092E\u0947\u0902 \u092A\u0930\u093F\
  \u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E\u0964 \u0921\u0947\
  \u0935\u0932\u092A\u0930\u094D\u0938 \u0907\u0938\u0947 \u092F\u0942\u091C\u093C\
  \u0930 \u0907\u0902\u091F\u0930\u092B\u0947\u0938 \u092E\u0947\u0902 \u0924\u093E\
  \u0930\u0940\u0916 \u0926\u093F\u0916\u093E\u0928\u0947, \u0932\u0949\u0917 \u092B\
  \u093E\u0907\u0932\u094D\u0938 \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916\
  \ \u0932\u093F\u0916\u0928\u0947, \u092F\u093E \u0921\u0947\u091F\u093E \u0915\u094B\
  \ \u0938\u0939\u0947\u091C\u0924\u0947 \u0914\u0930 \u092A\u0941\u0928\u0903 \u092A\
  \u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
weight: 28
---

## What & Why?
तारीख को स्ट्रिंग में बदलने का मतलब है एक तारीख ऑब्जेक्ट को पढ़ने लायक टेक्स्ट फॉर्मेट में परिवर्तित करना। डेवलपर्स इसे यूज़र इंटरफेस में तारीख दिखाने, लॉग फाइल्स में तारीख लिखने, या डेटा को सहेजते और पुनः प्राप्त करने के लिए करते हैं।

## How to:
```Javascript
let currentDateTime = new Date();
console.log(currentDateTime.toString()); // "Wed Mar 24 2021 11:28:34 GMT+0530 (India Standard Time)"

console.log(currentDateTime.toISOString()); // "2021-03-24T05:58:34.959Z"

console.log(currentDateTime.toLocaleDateString('hi-IN')); // "24/3/2021"

console.log(currentDateTime.toTimeString()); // "11:28:34 GMT+0530 (India Standard Time)"
```

## Deep Dive
तारीख को स्ट्रिंग में बदलने के कई तरीके हैं। सबसे पहले, `.toString()` से शुरू होते हैं जो जावास्क्रिप्ट के पुराने वर्ज़न्स से है। यह पूर्ण तारीख और समय दिखाता है। इसके बाद `.toISOString()` है, जो ISO-8601 फॉर्मेट में तारीख देता है - यह डाटाबेस स्टोरेज और API के लिए उपयोगी होता है। फिर `.toLocaleDateString()` है जो लोकल फॉर्मेटिंग के साथ तारीख देता है, अर्थात अलग-अलग भागों में अलग तरीके से तारीख दिखाना। मसलन, 'hi-IN' पारित करने पर, हिंदी (भारत) के लिए स्थानीयकृत तारीख प्राप्त होती है।
`.toTimeString()` सिर्फ समय भाग देता है। ये फंक्शन जावास्क्रिप्ट Date ऑब्जेक्ट की परिचालित परिभाषाओं का हिस्सा हैं और तारीख-संबंधित डेटा को मैनेज करने में विभिन्न परिदृश्यों के लिए उपयुक्त होते हैं।

## See Also
- MDN Web Docs for Date objects: [Date - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- ISO 8601 Date and time format: [ISO 8601 - Wikipedia](https://en.wikipedia.org/wiki/ISO_8601)
- Date and time formatting in JavaScript: [Date.prototype.toLocaleDateString() - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)

---
date: 2024-01-20 17:31:41.664690-07:00
description: "JavaScript \u092E\u0947\u0902 \u092D\u0935\u093F\u0937\u094D\u092F \u092F\
  \u093E \u0905\u0924\u0940\u0924 \u0915\u0940 \u0924\u093E\u0930\u0940\u0916 \u0915\
  \u0940 \u0917\u0923\u0928\u093E \u0915\u0930\u0928\u093E \u0938\u0930\u0932 \u0939\
  \u0948\u0964 \u0907\u0938\u0915\u0940 \u091C\u0930\u0942\u0930\u0924 \u091B\u0941\
  \u091F\u094D\u091F\u093F\u092F\u094B\u0902 \u0915\u0940 \u092F\u094B\u091C\u0928\
  \u093E, \u0938\u092E\u092F-\u0938\u0940\u092E\u093E \u0924\u092F \u0915\u0930\u0928\
  \u0947, \u092F\u093E \u092A\u0941\u0930\u093E\u0928\u0940 \u092B\u093E\u0907\u0932\
  \u094B\u0902 \u0915\u0947 \u0930\u093F\u0915\u093E\u0930\u094D\u0921\u094D\u200C\
  \u0938 \u0915\u094B \u0938\u0902\u092D\u093E\u0932\u0928\u0947 \u092E\u0947\u0902\
  \u2026"
lastmod: '2024-02-25T18:49:50.213858-07:00'
model: gpt-4-1106-preview
summary: "JavaScript \u092E\u0947\u0902 \u092D\u0935\u093F\u0937\u094D\u092F \u092F\
  \u093E \u0905\u0924\u0940\u0924 \u0915\u0940 \u0924\u093E\u0930\u0940\u0916 \u0915\
  \u0940 \u0917\u0923\u0928\u093E \u0915\u0930\u0928\u093E \u0938\u0930\u0932 \u0939\
  \u0948\u0964 \u0907\u0938\u0915\u0940 \u091C\u0930\u0942\u0930\u0924 \u091B\u0941\
  \u091F\u094D\u091F\u093F\u092F\u094B\u0902 \u0915\u0940 \u092F\u094B\u091C\u0928\
  \u093E, \u0938\u092E\u092F-\u0938\u0940\u092E\u093E \u0924\u092F \u0915\u0930\u0928\
  \u0947, \u092F\u093E \u092A\u0941\u0930\u093E\u0928\u0940 \u092B\u093E\u0907\u0932\
  \u094B\u0902 \u0915\u0947 \u0930\u093F\u0915\u093E\u0930\u094D\u0921\u094D\u200C\
  \u0938 \u0915\u094B \u0938\u0902\u092D\u093E\u0932\u0928\u0947 \u092E\u0947\u0902\
  \u2026"
title: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

JavaScript में भविष्य या अतीत की तारीख की गणना करना सरल है। इसकी जरूरत छुट्टियों की योजना, समय-सीमा तय करने, या पुरानी फाइलों के रिकार्ड्‌स को संभालने में पड़ सकती है। 

## कैसे करें:

```Javascript
// आज की तिथि
const today = new Date();

// 10 दिन बाद की तिथि
const tenDaysLater = new Date(today);
tenDaysLater.setDate(tenDaysLater.getDate() + 10);
console.log(tenDaysLater.toString()); // "Sun Apr 10 2023 12:00:00 GMT+0530 (India Standard Time)"

// 30 दिन पहले की तिथि
const thirtyDaysAgo = new Date(today);
thirtyDaysAgo.setDate(thirtyDaysAgo.getDate() - 30);
console.log(thirtyDaysAgo.toString()); // "Thu Mar 02 2023 12:00:00 GMT+0530 (India Standard Time)"
```

## गहराई से जानकारी:

तिथियों की गणना के लिए JavaScript में `Date` ऑब्जेक्ट शुरू से ही रहा है, लेकिन समय के साथ इसमें सुधार होते रहे हैं। अन्य तरीकों में, Moment.js और Date-fns जैसे लाइब्रेरीज शामिल हैं, लेकिन ज्यादा सरल गणना के लिए आमतौर पर इनकी जरूरत नहीं पड़ती। जब आप `setDate()` मेथड का उपयोग करते हैं, तो JavaScript खुद ही महीने और वर्ष को संभाल लेता है, अगर तारीखें उनकी सीमा को पार कर जाती हैं। यह समझने के लिए कि JavaScript किस तरह तारीखों को संभालता है, MDN Web Docs का उपयोग कर सकते हैं। 

## संबंधित स्रोत:

- MDN Web Docs on JavaScript Dates: [Mozilla MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- JavaScript Info on Date and Time: [JavaScript.info](https://javascript.info/date)
- Moment.js Library: [Moment.js](https://momentjs.com/)
- Date-fns Library: [Date-fns](https://date-fns.org/)

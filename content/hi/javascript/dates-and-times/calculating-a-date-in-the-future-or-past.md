---
date: 2024-01-20 17:31:41.664690-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0924\u093F\u0925\
  \u093F\u092F\u094B\u0902 \u0915\u0940 \u0917\u0923\u0928\u093E \u0915\u0947 \u0932\
  \u093F\u090F JavaScript \u092E\u0947\u0902 `Date` \u0911\u092C\u094D\u091C\u0947\
  \u0915\u094D\u091F \u0936\u0941\u0930\u0942 \u0938\u0947 \u0939\u0940 \u0930\u0939\
  \u093E \u0939\u0948, \u0932\u0947\u0915\u093F\u0928 \u0938\u092E\u092F \u0915\u0947\
  \ \u0938\u093E\u0925 \u0907\u0938\u092E\u0947\u0902 \u0938\u0941\u0927\u093E\u0930\
  \ \u0939\u094B\u0924\u0947 \u0930\u0939\u0947 \u0939\u0948\u0902\u0964 \u0905\u0928\
  \u094D\u092F \u0924\u0930\u0940\u0915\u094B\u0902 \u092E\u0947\u0902, Moment.js\u2026"
lastmod: '2024-04-05T21:53:54.956268-06:00'
model: gpt-4-1106-preview
summary: "\u0924\u093F\u0925\u093F\u092F\u094B\u0902 \u0915\u0940 \u0917\u0923\u0928\
  \u093E \u0915\u0947 \u0932\u093F\u090F JavaScript \u092E\u0947\u0902 `Date` \u0911\
  \u092C\u094D\u091C\u0947\u0915\u094D\u091F \u0936\u0941\u0930\u0942 \u0938\u0947\
  \ \u0939\u0940 \u0930\u0939\u093E \u0939\u0948, \u0932\u0947\u0915\u093F\u0928 \u0938\
  \u092E\u092F \u0915\u0947 \u0938\u093E\u0925 \u0907\u0938\u092E\u0947\u0902 \u0938\
  \u0941\u0927\u093E\u0930 \u0939\u094B\u0924\u0947 \u0930\u0939\u0947 \u0939\u0948\
  \u0902\u0964 \u0905\u0928\u094D\u092F \u0924\u0930\u0940\u0915\u094B\u0902 \u092E\
  \u0947\u0902, Moment.js \u0914\u0930 Date-fns \u091C\u0948\u0938\u0947 \u0932\u093E\
  \u0907\u092C\u094D\u0930\u0947\u0930\u0940\u091C \u0936\u093E\u092E\u093F\u0932\
  \ \u0939\u0948\u0902, \u0932\u0947\u0915\u093F\u0928 \u091C\u094D\u092F\u093E\u0926\
  \u093E \u0938\u0930\u0932 \u0917\u0923\u0928\u093E \u0915\u0947 \u0932\u093F\u090F\
  \ \u0906\u092E\u0924\u094C\u0930 \u092A\u0930 \u0907\u0928\u0915\u0940 \u091C\u0930\
  \u0942\u0930\u0924 \u0928\u0939\u0940\u0902 \u092A\u0921\u093C\u0924\u0940\u0964\
  \ \u091C\u092C \u0906\u092A `setDate()` \u092E\u0947\u0925\u0921 \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902, \u0924\u094B\
  \ JavaScript \u0916\u0941\u0926 \u0939\u0940 \u092E\u0939\u0940\u0928\u0947 \u0914\
  \u0930 \u0935\u0930\u094D\u0937 \u0915\u094B \u0938\u0902\u092D\u093E\u0932 \u0932\
  \u0947\u0924\u093E \u0939\u0948, \u0905\u0917\u0930 \u0924\u093E\u0930\u0940\u0916\
  \u0947\u0902 \u0909\u0928\u0915\u0940 \u0938\u0940\u092E\u093E \u0915\u094B \u092A\
  \u093E\u0930 \u0915\u0930 \u091C\u093E\u0924\u0940 \u0939\u0948\u0902\u0964 \u092F\
  \u0939 \u0938\u092E\u091D\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u093F\
  \ JavaScript \u0915\u093F\u0938 \u0924\u0930\u0939 \u0924\u093E\u0930\u0940\u0916\
  \u094B\u0902 \u0915\u094B \u0938\u0902\u092D\u093E\u0932\u0924\u093E \u0939\u0948\
  , MDN Web Docs \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\
  \u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E"
weight: 26
---

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

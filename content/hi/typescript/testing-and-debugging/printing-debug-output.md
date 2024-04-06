---
date: 2024-01-20 17:53:37.176518-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u094D\
  \u0930\u093F\u0902\u091F\u093F\u0902\u0917 \u0921\u0940\u092C\u0917 \u0906\u0909\
  \u091F\u092A\u0941\u091F \u092C\u0939\u0941\u0924 \u092A\u0941\u0930\u093E\u0928\
  \u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\u0964\
  \ `console.log()` \u091C\u093E\u0935\u093E\u0938\u094D\u0915\u094D\u0930\u093F\u092A\
  \u094D\u091F \u092E\u0947\u0902 \u0906\u092E \u0924\u094C\u0930 \u092A\u0930 \u0907\
  \u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\u093E \u0906\u092F\
  \u093E \u0939\u0948\u0964 \u0905\u0932\u094D\u091F\u0930\u0928\u0947\u091F\u093F\
  \u0935 \u0924\u094C\u0930 \u092A\u0930\u2026"
lastmod: '2024-04-05T22:51:06.563508-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u094D\u0930\u093F\
  \u0902\u091F\u093F\u0902\u0917 \u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\
  \u0941\u091F \u092C\u0939\u0941\u0924 \u092A\u0941\u0930\u093E\u0928\u0940 \u092A\
  \u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\u0964 `console.log()`\
  \ \u091C\u093E\u0935\u093E\u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F\
  \ \u092E\u0947\u0902 \u0906\u092E \u0924\u094C\u0930 \u092A\u0930 \u0907\u0938\u094D\
  \u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\u093E \u0906\u092F\u093E \u0939\
  \u0948\u0964 \u0905\u0932\u094D\u091F\u0930\u0928\u0947\u091F\u093F\u0935 \u0924\
  \u094C\u0930 \u092A\u0930 log levels \u091C\u0948\u0938\u0947 \u0915\u0940 `console.error()`,\
  \ `console.warn()` \u0914\u0930 `console.info()` \u0915\u093E \u0907\u0938\u094D\
  \u0924\u0947\u092E\u093E\u0932 \u0905\u0932\u0917-\u0905\u0932\u0917 \u092A\u094D\
  \u0930\u0915\u093E\u0930 \u0915\u0940 \u091C\u093E\u0928\u0915\u093E\u0930\u0940\
  \ \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u091C\u093E\u0935\u093E\u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F\
  \ debugging tools \u091C\u0948\u0938\u0947 \u0915\u093F Chrome DevTools \u092D\u0940\
  \ \u092C\u0921\u093C\u0940 \u092E\u0926\u0926 \u0915\u0930\u0924\u0947 \u0939\u0948\
  \u0902\u0964 TypeScript, \u091C\u094B \u090F\u0915 typed \u0938\u0941\u092A\u0930\
  \u0938\u0947\u091F \u0939\u0948 JavaScript \u0915\u093E, \u092E\u0947\u0902 \u092D\
  \u0940 \u0939\u092E \u0907\u0928\u094D\u0939\u0940 \u0924\u0930\u0940\u0915\u094B\
  \u0902 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\
  \ \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u0915\u0908 \u092C\u093E\u0930\
  , debugging \u0915\u0947 \u0932\u093F\u090F \u092C\u093E\u0939\u0930\u0940 \u0932\
  \u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940\u091C\u093C \u091C\u0948\u0938\u0947\
  \ ki `debug` \u092F\u093E `loglevel` \u092D\u0940 \u0909\u092A\u092F\u094B\u0917\
  \ \u092E\u0947\u0902 \u0932\u093E\u092F\u0940 \u091C\u093E\u0924\u0940 \u0939\u0948\
  \u0902\u0964."
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

## How to: (कैसे करें:)
```TypeScript
// साधारण console.log() का इस्तेमाल
console.log('Hello, debugging world!');

// वेरिएबल के मूल्य के साथ
let debugVariable = 'Testing value';
console.log('Debug Variable:', debugVariable);

// एक complex object को प्रिंट करने के लिए
let debugObject = { name: 'Test Object', id: 1 };
console.log('Debug Object:', debugObject);

// स्ट्रिंग इंटरपोलेशन या template literals का इस्तेमाल करके
console.log(`Variable content: ${debugVariable}`);
```
Sample Output:
```
Hello, debugging world!
Debug Variable: Testing value
Debug Object: { name: 'Test Object', id: 1 }
Variable content: Testing value
```

## Deep Dive (गहराई में जानकारी)
प्रिंटिंग डीबग आउटपुट बहुत पुरानी प्रक्रिया है। `console.log()` जावास्क्रिप्ट में आम तौर पर इस्तेमाल होता आया है। अल्टरनेटिव तौर पर log levels जैसे की `console.error()`, `console.warn()` और `console.info()` का इस्तेमाल अलग-अलग प्रकार की जानकारी के लिए करते हैं। जावास्क्रिप्ट debugging tools जैसे कि Chrome DevTools भी बड़ी मदद करते हैं। TypeScript, जो एक typed सुपरसेट है JavaScript का, में भी हम इन्ही तरीकों का इस्तेमाल कर सकते हैं। कई बार, debugging के लिए बाहरी लाइब्रेरीज़ जैसे ki `debug` या `loglevel` भी उपयोग में लायी जाती हैं।

## See Also (और भी जानकारी)
- [Console API Reference (MDN)](https://developer.mozilla.org/en-US/docs/Web/API/console)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [Node.js Debugging Guide](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Chrome DevTools Documentation](https://developers.google.com/web/tools/chrome-devtools)

---
date: 2024-01-20 17:53:37.176518-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Sample\
  \ Output."
lastmod: '2024-04-05T21:53:53.898967-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Sample Output."
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

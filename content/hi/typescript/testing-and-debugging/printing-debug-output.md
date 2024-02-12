---
title:                "डीबग आउटपुट प्रिंट करना"
aliases: - /hi/typescript/printing-debug-output.md
date:                  2024-01-20T17:53:37.176518-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Debug output प्रिंट करने का मतलब है कोड में जानकारी दिखाना ताकि समस्या का पता लगा सकें। प्रोग्रामर्स यह इसलिए करते हैं ताकि कोड की खोजबीन करते समय मुश्किलें आसानी से सुलझा सकें।

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

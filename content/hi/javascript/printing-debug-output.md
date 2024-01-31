---
title:                "डीबग आउटपुट प्रिंट करना"
date:                  2024-01-20T17:53:34.736152-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"

category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डीबगिंग आउटपुट प्रिंट करना यानी कोड से जानकारी को कंसोल पर दिखाना। यह डेवलपर्स को बग्स खोजने और कोड की समझ बढ़ाने में मदद करता है।

## How to: (कैसे करें:)
```javascript
// बेसिक उदाहरण
console.log('नमस्ते, यह एक डीबग मैसेज है');
// संख्याओं के साथ
let number = 42;
console.log('संख्या:', number);
// ऑब्जेक्ट प्रिंट करना
let person = { name: 'रोहन', age: 28 };
console.log('ऑब्जेक्ट:', person);
```
सैंपल आउटपुट:
```
नमस्ते, यह एक डीबग मैसेज है
संख्या: 42
ऑब्जेक्ट: { name: 'रोहन', age: 28 }
```

## Deep Dive (गहराई में जानकारी)
प्रिंट डीबग आउटपुट का इस्तेमाल जावास्क्रिप्ट के शुरुआती दिनों से हो रहा है। `console.log()` सबसे आम तरीका है, पर `console.warn()`, `console.error()`, और `console.info()` जैसे विकल्प भी हैं जो विभिन्न संदेशों के लिए हैं। नोड.जेएस में, डीबगिंग के और भी उन्नत तरीके हो सकते हैं जैसे कि `debugger` स्टेटमेंट जिसके जरिये ब्रेकपॉइंट सेट किये जा सकते हैं।

## See Also (और भी देखें)
- [Console API](https://developer.mozilla.org/en-US/docs/Web/API/Console) - MDN Web Docs में कंसोल एपीआई का प्रलेखन
- [Node.js Debugging Guide](https://nodejs.org/en/docs/guides/debugging-getting-started/) - नोड.जेएस डीबगिंग का आधिकारिक गाइड
- [Google Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools/) - गूगल क्रोम के डीवेलपर टूल्स पर डॉक्स

---
date: 2024-01-20 17:47:41.300108-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.974468-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u091C\u094D\u091E\u093E\u0924 \u0915\u0930\u0928\u093E"
weight: 7
---

## How to: (कैसे करें:)
```javascript
let greeting = 'नमस्ते दुनिया';
console.log(greeting.length); // आउटपुट: 13

let emptyString = '';
console.log(emptyString.length); // आउटपुट: 0
```
सिंपल है, है न? `.length` property का इस्तेमाल करके हम आसानी से string की length निकाल सकते हैं।

## Deep Dive (गहरी खोज)
पहले के वक्तों में, जब JavaScript नया-नया था, string handling इतनी आसान नहीं थी। अब `.length` property के साथ, हर string instance पर straightaway लेंथ चेक करना possible है।

Alternatives? खैर, mostly `.length` ही use होता है, पर looping और manual counting methods भी हैं जो अब obsolete हैं।

Implementation की बात करें तो, JavaScript internally UCS-2/UTF-16 जैसे encoding formats use करता है। इसका मतलब है कि surrogate pairs का count correct नहीं हो सकता। एक surrogate pair actually 2 units का होता है, पर `.length` उसे दो characters के रूप में ही गिनेगा। यह rare cases में problem create कर सकता है।

## See Also (और भी जानकारी)
- MDN Web Docs on JavaScript String length: [MDN String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- UTF-16 and JavaScript's internal character representation: [Understanding UTF-16](https://flaviocopes.com/javascript-utf-16/)

इन resources के ज़रिए आप और गहराई में जा सकते हैं और JavaScript strings के इन्टरनल्स समझ सकते हैं।

---
date: 2024-01-20 17:46:23.890279-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u0939\
  \u0932\u0947 \u0915\u0947 \u091C\u092E\u093E\u0928\u0947 \u092E\u0947\u0902, `substr()`\
  \ \u0914\u0930 `substring()` \u0938\u092C\u0938\u0947 \u0906\u092E \u0924\u0930\u0940\
  \u0915\u0947 \u0939\u094B\u0924\u0947 \u0925\u0947 substring \u0928\u093F\u0915\u093E\
  \u0932\u0928\u0947 \u0915\u0947. `substr` \u0905\u092A\u0928\u0940 \u0926\u0942\u0938\
  \u0930\u0940 argument \u092E\u0947\u0902 length\u2026"
lastmod: '2024-04-05T22:51:07.632860-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u0939\u0932\u0947\
  \ \u0915\u0947 \u091C\u092E\u093E\u0928\u0947 \u092E\u0947\u0902, `substr()` \u0914\
  \u0930 `substring()` \u0938\u092C\u0938\u0947 \u0906\u092E \u0924\u0930\u0940\u0915\
  \u0947 \u0939\u094B\u0924\u0947 \u0925\u0947 substring \u0928\u093F\u0915\u093E\u0932\
  \u0928\u0947 \u0915\u0947."
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
weight: 6
---

## How to: (कैसे करें:)
```javascript
let fullString = "नमस्ते Javascript वर्ल्ड!";
// substr(startIndex, length)
let substr1 = fullString.substr(7, 10);
console.log(substr1);  // Javascript

// substring(startIndex, endIndex)
let substr2 = fullString.substring(7, 17);
console.log(substr2);  // Javascript

// slice(startIndex, endIndex)
let substr3 = fullString.slice(7, 17);
console.log(substr3);  // Javascript
```
=> सैम्पल आउटपुट:
```
Javascript
Javascript
Javascript
```

## Deep Dive (गहरी जानकारी)
पहले के जमाने में, `substr()` और `substring()` सबसे आम तरीके होते थे substring निकालने के. `substr` अपनी दूसरी argument में length मांगता है, जबकि `substring` में startIndex और endIndex मांगे जाते हैं. हालांकि ECMA ने `substr` को deprecated कर दिया है, यानी आगे चल कर ये हट जाएगा. इसीलिए `slice()` ज्यादा बढ़िया choice है - ये `substring()` की तरह ही startIndex और endIndex लेता है, और negative indices को भी support करता है.

## See Also (और जानकारी के लिए)
- MDN Web Docs पर [String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- MDN Web Docs पर [String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- MDN Web Docs पर [String.prototype.substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr) (Deprecated)

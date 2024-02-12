---
title:                "सबस्ट्रिंग्स निकालना"
aliases:
- /hi/javascript/extracting-substrings/
date:                  2024-01-20T17:46:23.890279-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
जावास्क्रिप्ट में substring निकालना यानी किसी स्ट्रिंग के खास हिस्से को अलग करना. प्रोग्रामर्स ऐसा डेटा में से जरूरी जानकारी छांटने के लिए करते हैं.

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

---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:15:48.470981-07:00
html_title:           "C: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
जावास्क्रिप्ट में वर्तमान तारीख (current date) प्राप्त करना वक्त के साथ डेटा के संबंध बनाना है। डेवेलपर्स इसे लॉग्स, यूजर्स की एक्टिविटी और टाइम-सेंसिटिव फीचर्स के लिए इस्तेमाल करते हैं। 

## How to: (कैसे करें:)
```javascript
// वर्तमान तारीख और समय प्राप्त करना
const now = new Date();
console.log(now);
// Output कुछ इस तरह हो सकता है: Thu Mar 31 2022 19:45:00 GMT+0530 (India Standard Time)

// केवल वर्तमान दिनांक प्राप्त करना
console.log(now.toDateString());
// Output: 'Thu Mar 31 2022'

// केवल वर्तमान समय प्राप्त करना
console.log(now.toTimeString());
// Output: '19:45:00 GMT+0530 (India Standard Time)'
```

## Deep Dive (गहराई में जानकारी)
जावास्क्रिप्ट में तारीख की माहिती `Date` ऑब्जेक्ट से आती है जो 1970 से मिलान करता है। इसमें मिलीसेकंड्स को माप कर समय को प्रस्तुत किया जाता है।

वैकल्पिक तरीकों में `moment.js`, `date-fns` जैसी लाइब्रेरीज़ हैं, पर `Date` ऑब्जेक्ट की सादगी इसे लोकप्रिय बनाती है। 

हम `Date` का इस्तेमाल कर के केवल वर्ष, महीना, हफ्ते के दिन आदि प्राप्त कर सकते हैं। जावास्क्रिप्ट रनटाइम की लोकल समय-सेटिंग्स और टाइमज़ोन के हिसाब से `Date` के फ़ंक्शन्स बदलते हैं।

## See Also (और देखें)
- MDN Web Docs on Date: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Moment.js: [https://momentjs.com/](https://momentjs.com/)
- date-fns library: [https://date-fns.org/](https://date-fns.org/) 

इन लिंक्स से आप `Date` ऑब्जेक्ट और इसके साथ काम करने के और भी तरीके सीख सकते हैं।
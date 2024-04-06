---
date: 2024-01-20 17:34:05.476083-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) \u0924\u093E\
  \u0930\u0940\u0916 \u0915\u0940 \u0924\u0941\u0932\u0928\u093E \u091C\u093E\u0935\
  \u093E\u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F \u092E\u0947\u0902\
  \ `Date` \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F\u094D\u0938 \u0915\u0947\
  \ \u0938\u093E\u0925 \u0915\u0940 \u091C\u093E \u0938\u0915\u0924\u0940 \u0939\u0948\
  \u0964 \u0928\u0940\u091A\u0947 \u0926\u0947\u0916\u0947\u0902 \u0915\u0948\u0938\
  \u0947."
lastmod: '2024-04-05T22:38:53.890124-06:00'
model: gpt-4-1106-preview
summary: ") \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0924\u0941\u0932\u0928\u093E\
  \ \u091C\u093E\u0935\u093E\u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F\
  \ \u092E\u0947\u0902 `Date` \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F\u094D\
  \u0938 \u0915\u0947 \u0938\u093E\u0925 \u0915\u0940 \u091C\u093E \u0938\u0915\u0924\
  \u0940 \u0939\u0948\u0964 \u0928\u0940\u091A\u0947 \u0926\u0947\u0916\u0947\u0902\
  \ \u0915\u0948\u0938\u0947."
title: "\u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E"
weight: 27
---

## कैसे करें? (How to:)
तारीख की तुलना जावास्क्रिप्ट में `Date` ऑब्जेक्ट्स के साथ की जा सकती है। नीचे देखें कैसे:

```javascript
// दो तारीखें लेते हैं
const date1 = new Date('2023-01-01');
const date2 = new Date('2023-07-01');

// तुलना करना
if(date1 < date2) {
  console.log('पहली तारीख दूसरी तारीख से पहले है।');
} else if(date1 > date2) {
  console.log('पहली तारीख दूसरी तारीख के बाद है।');
} else {
  console.log('दोनों तारीखें समान हैं।');
}
```
Output होगा:
```
पहली तारीख दूसरी तारीख से पहले है।
```

## गहराई में जानकारी (Deep Dive)
जावास्क्रिप्ट में `Date` ऑब्जेक्ट 1 जनवरी, 1970 को मिडनाइट UTC से मिलीसेकंड्स में समय रखता है। इसको टाइम स्टैम्प कहा जाता है। तारीखों की तुलना, दरअसल, इन टाइम स्टैम्प्स की तुलना होती है।

तारीखों की तुलना में विकल्प भी हैं जैसे कि `moment.js` लेकिन वह पुस्तकालय अब deprecated है, और `day.js` या `date-fns` जैसे लाइब्रेरीज अधिक लोकप्रिय हो रहे हैं।

कभी-कभी टाइमज़ोन के मुद्दे हो सकते हैं, ऐसे में `toISOString()` या `getTime()` मदद कर सकते हैं क्योंकि वे UTC में समय लौटाते हैं।

## इसे भी देखें (See Also)
- MDN Web Docs पर JavaScript `Date` ऑब्जेक्ट: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- `date-fns` लाइब्रेरी: [https://date-fns.org/](https://date-fns.org/)
- `day.js` लाइब्रेरी: [https://day.js.org/](https://day.js.org/)
- टाइमज़ोन के बारे में ज्यादा जानने के लिए: [https://moment.github.io/luxon/#/zones?id=whymightiwanttoknowthetimezone](https://moment.github.io/luxon/#/zones?id=whymightiwanttoknowthetimezone)

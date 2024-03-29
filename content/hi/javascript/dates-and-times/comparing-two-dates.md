---
date: 2024-01-20 17:34:05.476083-07:00
description: "\u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\u0941\
  \u0932\u0928\u093E \u0915\u0930\u0928\u093E \u0915\u093E \u092E\u0924\u0932\u092C\
  \ \u0939\u0948 \u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u094B\
  \ \u0906\u092A\u0938 \u092E\u0947\u0902 \u0926\u0947\u0916\u0928\u093E \u0915\u093F\
  \ \u0915\u094C\u0928\u0938\u0940 \u092A\u0939\u0932\u0947 \u0939\u0948\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\u0939\
  \ \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0924\
  \u093E\u0915\u093F \u0935\u094B \u0938\u092E\u092F \u0938\u0947 \u091C\u0941\u095C\
  \u0947 \u0932\u0949\u091C\u093F\u0915 \u091C\u0948\u0938\u0947 \u0915\u093F \u0907\
  \u0935\u0947\u0902\u091F\u094D\u0938 \u0915\u0940\u2026"
lastmod: '2024-03-13T22:44:53.011153-06:00'
model: gpt-4-1106-preview
summary: "\u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\u0941\u0932\
  \u0928\u093E \u0915\u0930\u0928\u093E \u0915\u093E \u092E\u0924\u0932\u092C \u0939\
  \u0948 \u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u094B \u0906\
  \u092A\u0938 \u092E\u0947\u0902 \u0926\u0947\u0916\u0928\u093E \u0915\u093F \u0915\
  \u094C\u0928\u0938\u0940 \u092A\u0939\u0932\u0947 \u0939\u0948\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\u0939 \u0907\
  \u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0924\u093E\
  \u0915\u093F \u0935\u094B \u0938\u092E\u092F \u0938\u0947 \u091C\u0941\u095C\u0947\
  \ \u0932\u0949\u091C\u093F\u0915 \u091C\u0948\u0938\u0947 \u0915\u093F \u0907\u0935\
  \u0947\u0902\u091F\u094D\u0938 \u0915\u0940\u2026"
title: "\u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
तारीखों की तुलना करना का मतलब है दो तारीखों को आपस में देखना कि कौनसी पहले है। प्रोग्रामर्स यह इसलिए करते हैं ताकि वो समय से जुड़े लॉजिक जैसे कि इवेंट्स की सीक्वेंसिंग, एज सोर्टिंग, या समय की अवधि की गणना कर सकें।

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

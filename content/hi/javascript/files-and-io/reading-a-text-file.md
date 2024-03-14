---
date: 2024-01-20 17:55:23.251371-07:00
description: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E \u092E\u0924\u0932\u092C \u092B\u093E\u0907\u0932\
  \ \u0938\u0947 \u0932\u0947\u0916\u0928 (text) \u0915\u094B \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E \u092E\u0947\u0902 \u0932\u093E\u0928\u093E \u0939\
  \u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\
  \u0938\u0947 \u0915\u0949\u0928\u094D\u092B\u093F\u0917\u0930\u0947\u0936\u0928\
  , \u0921\u0947\u091F\u093E \u090F\u0928\u093E\u0932\u093F\u0938\u093F\u0938, \u0914\
  \u0930 \u0932\u094B\u0917 \u092B\u093E\u0907\u0932\u094D\u0938 \u091C\u0948\u0938\
  \u0947 \u0915\u093E\u092E\u094B\u0902 \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902\u0964"
lastmod: '2024-03-13T22:44:53.019661-06:00'
model: gpt-4-1106-preview
summary: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093E\u0907\u0932 \u092A\
  \u0922\u093C\u0928\u093E \u092E\u0924\u0932\u092C \u092B\u093E\u0907\u0932 \u0938\
  \u0947 \u0932\u0947\u0916\u0928 (text) \u0915\u094B \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E \u092E\u0947\u0902 \u0932\u093E\u0928\u093E \u0939\u0948\
  \u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\
  \u0947 \u0915\u0949\u0928\u094D\u092B\u093F\u0917\u0930\u0947\u0936\u0928, \u0921\
  \u0947\u091F\u093E \u090F\u0928\u093E\u0932\u093F\u0938\u093F\u0938, \u0914\u0930\
  \ \u0932\u094B\u0917 \u092B\u093E\u0907\u0932\u094D\u0938 \u091C\u0948\u0938\u0947\
  \ \u0915\u093E\u092E\u094B\u0902 \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\u0924\
  \u0947 \u0939\u0948\u0902\u0964"
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
टेक्स्ट फाइल पढ़ना मतलब फाइल से लेखन (text) को प्रोग्राम में लाना है। प्रोग्रामर इसे कॉन्फिगरेशन, डेटा एनालिसिस, और लोग फाइल्स जैसे कामों के लिए करते हैं।

## कैसे करें? (How to:)
Javascript में टेक्स्ट फाइल पढ़ने के लिए आप Node.js की फाइल सिस्टम मॉड्यूल (fs) का इस्तेमाल कर सकते हैं:

```javascript
const fs = require('fs');

// एसिंक्रोनस रीडिंग
fs.readFile('example.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});

// सिंक्रोनस रीडिंग
try {
  const data = fs.readFileSync('example.txt', 'utf8');
  console.log(data);
} catch (err) {
  console.error(err);
}
```

सैंपल आउटपुट:

```plaintext
Hello, this is text inside the example.txt file!
```

## गहराई से जानकारी (Deep Dive)
पहले, फाइल रीडिंग कमांड-लाइन टूल्स और बेसिक स्क्रिप्ट्स से की जाती थी। Node.js आने के बाद, जावास्क्रिप्ट में यह काम आसान हुआ। fs मॉड्यूल में ढेरों फंक्शन्स हैं जैसे `readFile` और `readFileSync` जो फाइल पढ़ने के लिए हैं। ये फंक्शन्स बाइनरी डेटा (Buffer) या स्ट्रिंग फॉर्म में डेटा देते हैं। `readFile` एसिंक्रोनस है जबकि `readFileSync` सिंक्रोनस है। एक बड़ी फाइल को पढ़ने के लिए स्ट्रीम्स का इस्तेमाल होता है ताकि मेमरी का कुशल इस्तेमाल हो।

## देखें भी (See Also)
- Node.js fs मॉड्यूल: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- जावास्क्रिप्ट प्रॉमिसेस: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- फाइल स्ट्रीम्स के बारे में: [https://nodejs.org/api/stream.html](https://nodejs.org/api/stream.html)

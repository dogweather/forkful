---
date: 2024-01-20 17:44:41.227381-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u0939\
  \u0932\u0947 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 FTP \u092F\
  \u093E Telnet \u091C\u0948\u0938\u0947 \u091F\u0942\u0932\u094D\u0938 \u0938\u0947\
  \ \u092E\u0948\u0928\u0941\u0905\u0932\u0940 \u092A\u0947\u091C \u0921\u093E\u0909\
  \u0928\u0932\u094B\u0921 \u0915\u0930\u0924\u0947 \u0925\u0947\u0964 \u0905\u092C\
  , Node.js \u091C\u0948\u0938\u0947 \u092A\u094D\u0932\u0947\u091F\u092B\u0949\u0930\
  \u094D\u092E\u094D\u0938 \u092A\u0930 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928\
  \ http \u0914\u0930 https\u2026"
lastmod: '2024-04-05T22:51:07.647520-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u0939\u0932\u0947\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 FTP \u092F\u093E\
  \ Telnet \u091C\u0948\u0938\u0947 \u091F\u0942\u0932\u094D\u0938 \u0938\u0947 \u092E\
  \u0948\u0928\u0941\u0905\u0932\u0940 \u092A\u0947\u091C \u0921\u093E\u0909\u0928\
  \u0932\u094B\u0921 \u0915\u0930\u0924\u0947 \u0925\u0947\u0964 \u0905\u092C, Node.js\
  \ \u091C\u0948\u0938\u0947 \u092A\u094D\u0932\u0947\u091F\u092B\u0949\u0930\u094D\
  \u092E\u094D\u0938 \u092A\u0930 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 http\
  \ \u0914\u0930 https \u092E\u0949\u0921\u094D\u092F\u0942\u0932\u094D\u0938 \u0909\
  \u0928\u094D\u0939\u0947\u0902 \u0911\u091F\u094B\u092E\u0947\u091F \u0915\u0930\
  \u0928\u0947 \u0914\u0930 \u0938\u094D\u091F\u094D\u0930\u0940\u092E\u094D\u0938\
  \ \u0915\u0947 \u091C\u0930\u093F\u090F \u092C\u0921\u093C\u0947 \u0921\u0947\u091F\
  \u093E \u0915\u094B \u0907\u092B\u0947\u0915\u094D\u091F\u093F\u0935\u0932\u0940\
  \ \u0939\u0948\u0902\u0921\u0932 \u0915\u0930\u0928\u0947 \u0915\u0940 \u0938\u0939\
  \u0942\u0932\u093F\u092F\u0924 \u0926\u0947\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u0905\u0928\u094D\u092F \u0935\u093F\u0915\u0932\u094D\u092A\u094B\u0902 \u092E\
  \u0947\u0902 `axios`, `request`, \u092F\u093E `fetch` \u091C\u0948\u0938\u0947 \u0932\
  \u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940\u091C\u093C \u0936\u093E\u092E\u093F\
  \u0932 \u0939\u0948\u0902, \u091C\u094B \u0905\u0927\u093F\u0915 \u092B\u0940\u091A\
  \u0930\u094D\u0938 \u0914\u0930 \u092A\u094D\u0930\u094B\u092E\u093F\u0938 \u092C\
  \u0947\u0938\u094D\u0921 API \u0926\u0947\u0924\u0947 \u0939\u0948\u0902\u0964 \u0938\
  \u094D\u0915\u094D\u0930\u0947\u092A\u093F\u0902\u0917 \u0915\u0930\u0924\u0947\
  \ \u0938\u092E\u092F \u0935\u0947\u092C\u0938\u093E\u0907\u091F \u0915\u0940 \u091F\
  \u0930\u094D\u092E\u094D\u0938 \u0911\u095E \u0938\u0930\u094D\u0935\u093F\u0938\
  \ \u0914\u0930 \u0932\u0940\u0917\u0932 \u0930\u0947\u0917\u0941\u0932\u0947\u0936\
  \u0928\u094D\u0938 \u0915\u093E \u0927\u094D\u092F\u093E\u0928 \u0930\u0916\u0947\
  \u0902\u0964."
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
weight: 42
---

## How to: (कैसे करें:)
```javascript
const https = require('https');
const fs = require('fs');

const downloadPage = (url, destination) => {
  https.get(url, response => {
    const file = fs.createWriteStream(destination);
    response.pipe(file);
    file.on('finish', () => {
      file.close();
      console.log('Download complete.');
    });
  }).on('error', error => {
    console.error('Error during download:', error);
  });
};

downloadPage('https://example.com', 'downloaded_page.html');
```

```plaintext
Download complete.
```

## Deep Dive (गहराई से जानकारी):
पहले प्रोग्रामर FTP या Telnet जैसे टूल्स से मैनुअली पेज डाउनलोड करते थे। अब, Node.js जैसे प्लेटफॉर्म्स पर बिल्ट-इन http और https मॉड्यूल्स उन्हें ऑटोमेट करने और स्ट्रीम्स के जरिए बड़े डेटा को इफेक्टिवली हैंडल करने की सहूलियत देते हैं। अन्य विकल्पों में `axios`, `request`, या `fetch` जैसे लाइब्रेरीज़ शामिल हैं, जो अधिक फीचर्स और प्रोमिस बेस्ड API देते हैं। स्क्रेपिंग करते समय वेबसाइट की टर्म्स ऑफ़ सर्विस और लीगल रेगुलेशन्स का ध्यान रखें।

## See Also (अधिक जानकारी के लिए):
- Node.js `https` मॉड्यूल का डॉक्यूमेंटेशन: https://nodejs.org/api/https.html
- Node.js `fs` (Filesystem) मॉड्यूल का डॉक्यूमेंटेशन: https://nodejs.org/api/fs.html
- `axios` का GitHub रेपो: https://github.com/axios/axios
- `fetch` API मॉडर्न उपयोग: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API

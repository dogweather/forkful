---
date: 2024-01-20 17:44:41.227381-07:00
description: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\
  \u094B\u0921 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0909\u0938\u0915\
  \u093E HTML \u0915\u094B\u0921 \u0905\u092A\u0928\u0947 \u0915\u0902\u092A\u094D\
  \u092F\u0942\u091F\u0930 \u092A\u0930 \u0932\u093E\u0928\u093E \u0939\u0948\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947\
  \ \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0921\u0947\u091F\u093E \u0915\u094B\
  \ \u092A\u0922\u093C\u0928\u0947, \u0938\u094D\u0915\u094D\u0930\u0947\u092A \u0915\
  \u0930\u0928\u0947 \u092F\u093E \u0935\u0947\u092C\u0938\u093E\u0907\u091F \u0915\
  \u0947 \u0915\u093E\u092E\u0915\u093E\u091C \u0915\u094B \u0938\u092E\u091D\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F\u0964"
lastmod: '2024-02-25T18:49:50.188904-07:00'
model: gpt-4-1106-preview
summary: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0909\u0938\u0915\u093E\
  \ HTML \u0915\u094B\u0921 \u0905\u092A\u0928\u0947 \u0915\u0902\u092A\u094D\u092F\
  \u0942\u091F\u0930 \u092A\u0930 \u0932\u093E\u0928\u093E \u0939\u0948\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0915\
  \u0930\u0924\u0947 \u0939\u0948\u0902 \u0921\u0947\u091F\u093E \u0915\u094B \u092A\
  \u0922\u093C\u0928\u0947, \u0938\u094D\u0915\u094D\u0930\u0947\u092A \u0915\u0930\
  \u0928\u0947 \u092F\u093E \u0935\u0947\u092C\u0938\u093E\u0907\u091F \u0915\u0947\
  \ \u0915\u093E\u092E\u0915\u093E\u091C \u0915\u094B \u0938\u092E\u091D\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F\u0964"
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वेब पेज डाउनलोड करना मतलब उसका HTML कोड अपने कंप्यूटर पर लाना है। प्रोग्रामर इसे करते हैं डेटा को पढ़ने, स्क्रेप करने या वेबसाइट के कामकाज को समझने के लिए।

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

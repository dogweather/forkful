---
title:                "वेब पेज डाउनलोड करना"
date:                  2024-01-20T17:44:41.227381-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/downloading-a-web-page.md"
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

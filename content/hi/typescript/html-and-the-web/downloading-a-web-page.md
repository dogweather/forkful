---
date: 2024-01-20 17:45:31.623785-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Node.js\
  \ \u0914\u0930 \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0932\
  \u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 Axios \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930\u0915\u0947 TypeScript \u092E\u0947\u0902 \u0935\
  \u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\u0921 \u0915\
  \u0930\u0928\u093E \u0938\u0930\u0932 \u0939\u0948\u0964 \u092A\u0939\u0932\u0947\
  \ `axios` \u0914\u0930 `@types/axios` \u0915\u094B\u2026"
lastmod: '2024-03-13T22:44:51.886912-06:00'
model: gpt-4-1106-preview
summary: "Node.js \u0914\u0930 \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\u093F\
  \u092F \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 Axios \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 TypeScript \u092E\u0947\
  \u0902 \u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E \u0938\u0930\u0932 \u0939\u0948\u0964 \u092A\u0939\
  \u0932\u0947 `axios` \u0914\u0930 `@types/axios` \u0915\u094B \u0907\u0902\u0938\
  \u094D\u091F\u0949\u0932 \u0915\u0930\u0947\u0902."
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
weight: 42
---

## How to: (कैसे करें:)
Node.js और एक लोकप्रिय लाइब्रेरी Axios का उपयोग करके TypeScript में वेब पेज डाउनलोड करना सरल है। पहले `axios` और `@types/axios` को इंस्टॉल करें:

```sh
npm install axios @types/axios
```

अब TypeScript में कोड लिखें:

```TypeScript
import axios from 'axios';

async function downloadWebPage(url: string): Promise<void> {
  try {
    const response = await axios.get(url);
    console.log(response.data);
  } catch (error) {
    console.error('Error downloading the page:', error);
  }
}

downloadWebPage('https://www.example.com');
```

आउटपुट (`response.data`) वह HTML होगा जो उस वेब पेज पर मौजूद है।

## Deep Dive (गहराई से जानकारी):
जब से वेब का आविष्कार हुआ, पेज डाउनलोड करने की जरुरत पड़ी। पहले `wget` और `curl` जैसे टूल काम में लाए जाते थे, अब प्रोग्रामिंग भाषाओं में लाइब्रेरीज़ उपलब्ध हैं। Axios विश्वसनीयता और मल्टीपल फीचर्स की वजह से पसंद की जाती है। Node.js के `http` मॉड्यूल जैसे नेटिव विकल्प भी हैं, पर Axios अधिक सुविधाजनक है।

Axios HTTP client serverside और clientside JavaScript में काम आती है, Promises पर आधारित है और error handling शानदार है। यह JSON data के साथ अच्छे से काम करती है और configuration आसान है।

## See Also (और देखें):
- Axios GitHub repository: [https://github.com/axios/axios](https://github.com/axios/axios)
- Node.js `http` module documentation: [https://nodejs.org/api/http.html](https://nodejs.org/api/http.html)
- MDN Web Docs - Fetch API: [https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- `wget` command guide: [https://www.gnu.org/software/wget/](https://www.gnu.org/software/wget/)
- `curl` command tutorial: [https://curl.se/docs/manual.html](https://curl.se/docs/manual.html)

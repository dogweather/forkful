---
title:                "वेब पेज डाउनलोड करना"
date:                  2024-01-20T17:45:31.623785-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

वेब पेज डाउनलोड करना मतलब उस पेज की सामग्री को इंटरनेट से आपके कंप्यूटर पर ले आना है। प्रोग्रामर्स इसे डेटा निकालने, टेस्टिंग और ऑफलाइन ब्राउजिंग के लिए करते हैं।

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
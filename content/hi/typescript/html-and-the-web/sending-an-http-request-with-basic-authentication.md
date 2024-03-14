---
date: 2024-01-20 18:02:57.483838-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u0915\u0947 \u0938\u093E\u0925\
  \ \u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u092F\u0942\u091C\u0930-\u0928\u0947\u092E \u0914\u0930 \u092A\u093E\
  \u0938\u0935\u0930\u094D\u0921 \u0915\u094B \u0915\u094B\u0921 \u0915\u0930 \u0915\
  \u0947 \u0938\u0930\u094D\u0935\u0930 \u0915\u094B \u092D\u0947\u091C\u0928\u0947\
  \ \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\
  \u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0907\u0938\u0947 \u0938\u0941\u0930\u0915\u094D\u0937\u093F\u0924 API \u090F\
  \u0902\u0921\u092A\u0949\u0907\u0902\u091F\u094D\u0938 \u0938\u0947 \u091C\u0941\
  \u0921\u093C\u0928\u0947 \u0915\u0947\u2026"
lastmod: '2024-03-13T22:44:51.888605-06:00'
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u0915\u0947 \u0938\u093E\u0925\
  \ \u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u092F\u0942\u091C\u0930-\u0928\u0947\u092E \u0914\u0930 \u092A\u093E\
  \u0938\u0935\u0930\u094D\u0921 \u0915\u094B \u0915\u094B\u0921 \u0915\u0930 \u0915\
  \u0947 \u0938\u0930\u094D\u0935\u0930 \u0915\u094B \u092D\u0947\u091C\u0928\u0947\
  \ \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\
  \u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0907\u0938\u0947 \u0938\u0941\u0930\u0915\u094D\u0937\u093F\u0924 API \u090F\
  \u0902\u0921\u092A\u0949\u0907\u0902\u091F\u094D\u0938 \u0938\u0947 \u091C\u0941\
  \u0921\u093C\u0928\u0947 \u0915\u0947\u2026"
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTTP अनुरोध के साथ बेसिक प्रमाणीकरण यूजर-नेम और पासवर्ड को कोड कर के सर्वर को भेजने की प्रक्रिया है। प्रोग्रामर्स इसे सुरक्षित API एंडपॉइंट्स से जुड़ने के लिए इस्तेमाल करते हैं।

## How to (कैसे करें):
TypeScript का उपयोग करके HTTP अनुरोध भेजने के लिए, हम axios जैसे लायब्रेरी का उपयोग करेंगे। पहले, आपको इसे इन्स्टॉल करना होगा:

```sh
npm install axios
```

फिर, हम बेसिक प्रमाणीकरण के साथ एक अनुरोध भेज सकते हैं:

```typescript
import axios from 'axios';

async function sendRequest() {
  const url = 'https://api.example.com/data';
  const username = 'yourUsername';
  const password = 'yourPassword';

  const response = await axios.get(url, {
    auth: {
      username,
      password
    }
  });

  console.log(response.data);
}

sendRequest();
```

सैंपल आउटपुट:

```json
{
  "data": "some secured data"
}
```

## Deep Dive (गहराई से जानकारी):
बेसिक प्रमाणीकरण, जिसे Basic Access Authentication भी कहा जाता है, HTTP प्रोटोकॉल में एक पुराना तरीका है। इसमें `Authorization` हेडर में `username:password` को Base64 इनकोडिंग में भेजा जाता है। 

हालांकि यह बहुत आसान है, इसमें कमजोरियां होती हैं क्योंकि Base64 इनकोडेड स्ट्रिंग आसानी से डीकोड की जा सकती है। इसलिए, HTTPS का इस्तेमाल जरूरी है, जो कि डाटा को एनक्रिप्ट करता है।

वर्तमान में, अधिक सुरक्षित प्रमाणीकरण विधियां जैसे OAuth 2.0 और JWT (JSON Web Tokens) पसंद की जा रही है। वे टोकन बेस्ड प्रमाणीकरण प्रदान करते हैं जो सत्र-प्रमाणीकरण से ज्यादा सुरक्षित और लचीला हैं।

जब आप TypeScript में axios का इस्तेमाल करते हैं, तो टाइप सुरक्षा का भी लाभ मिलता है। आप अपने अनुरोध और प्रतिक्रिया संरचनाओं को स्पष्ट करके बग्स से बच सकते हैं और code को बेहतर संरचना दे सकते हैं।

## See Also (और देखें):
- MDN Web Docs का Basic authentication गाइड: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme
- Axios डॉक्यूमेंटेशन: https://axios-http.com/docs/intro
- JWT और OAuth 2.0 के बारे में जानकारी: https://auth0.com/docs/authentication
- TypeScript हैंडबुक: https://www.typescriptlang.org/docs/

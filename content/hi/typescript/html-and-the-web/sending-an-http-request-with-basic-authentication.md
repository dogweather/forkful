---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
date:                  2024-01-20T18:02:57.483838-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/sending-an-http-request-with-basic-authentication.md"
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

---
date: 2024-01-20 18:01:51.536840-07:00
description: "HTTP request \u0915\u0947 \u0938\u093E\u0925 basic authentication \u090F\
  \u0915 \u0938\u0930\u0932 \u0924\u0930\u0940\u0915\u093E \u0939\u0948 \u091C\u093F\
  \u0938\u092E\u0947\u0902 username \u0914\u0930 password \u0915\u094B encode \u0915\
  \u0930\u0915\u0947 request headers \u092E\u0947\u0902 \u092D\u0947\u091C\u093E \u091C\
  \u093E\u0924\u093E \u0939\u0948\u0964 \u092F\u0939 \u0924\u0915\u0928\u0940\u0915\
  \ \u0935\u0947\u092C\u2026"
lastmod: '2024-03-13T22:44:52.989098-06:00'
model: gpt-4-1106-preview
summary: "HTTP request \u0915\u0947 \u0938\u093E\u0925 basic authentication \u090F\
  \u0915 \u0938\u0930\u0932 \u0924\u0930\u0940\u0915\u093E \u0939\u0948 \u091C\u093F\
  \u0938\u092E\u0947\u0902 username \u0914\u0930 password \u0915\u094B encode \u0915\
  \u0930\u0915\u0947 request headers \u092E\u0947\u0902 \u092D\u0947\u091C\u093E \u091C\
  \u093E\u0924\u093E \u0939\u0948\u0964 \u092F\u0939 \u0924\u0915\u0928\u0940\u0915\
  \ \u0935\u0947\u092C services \u0914\u0930 APIs \u0924\u0915 \u0938\u0941\u0930\u0915\
  \u094D\u0937\u093F\u0924 \u090F\u0915\u094D\u0938\u0947\u0938 \u092A\u093E\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F \u092A\u094D\u0930\u092F\u094B\u0917 \u0915\
  \u0940 \u091C\u093E\u0924\u0940 \u0939\u0948\u0964."
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

## क्या और क्यों?
HTTP request के साथ basic authentication एक सरल तरीका है जिसमें username और password को encode करके request headers में भेजा जाता है। यह तकनीक वेब services और APIs तक सुरक्षित एक्सेस पाने के लिए प्रयोग की जाती है।

## कैसे करें:
```javascript
const axios = require('axios');
const base64 = require('base-64');

let username = 'yourUsername';
let password = 'yourPassword';
let basicAuth = 'Basic ' + base64.encode(username + ':' + password);

axios.get('http://your-api-url.com/data', { headers: { 'Authorization': basicAuth } })
  .then(response => {
    console.log('Data fetched successfully:', response.data);
  })
  .catch(error => {
    console.error('Error fetching data:', error);
  });
```
Sample Output:
```
Data fetched successfully: { id: 1, title: 'Example Data', description: 'This is a JSON object.' }
```

## गहराई से समझिए:
Basic authentication का concept '90s के शुरुआती दिनों से HTTP protocol का हिस्सा है। यह Base64 encoding का इस्तेमाल करता है, जो कि बहुत सुरक्षित नहीं है, और इसलिए HTTPS का उपयोग जरूरी है। इसके अलावा, प्रोग्रामर OAuth जैसे अधिक सुरक्षित तरीकों का भी उपयोग कर सकते हैं। Basic authentication साधारण और जल्दी स्थापित होने के कारण छोटे applications या उन परियोजनाओं में पसंद किया जाता है जहां high-level security की दरकार नहीं होती है। 

## सम्बंधित सूत्र:
- MDN Web Docs on Authorization: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization
- Axios Library: https://github.com/axios/axios
- Base-64 npm package: https://www.npmjs.com/package/base-64

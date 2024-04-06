---
date: 2024-01-20 18:01:51.536840-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Basic authentication\
  \ \u0915\u093E concept '90s \u0915\u0947 \u0936\u0941\u0930\u0941\u0906\u0924\u0940\
  \ \u0926\u093F\u0928\u094B\u0902 \u0938\u0947 HTTP protocol \u0915\u093E \u0939\u093F\
  \u0938\u094D\u0938\u093E \u0939\u0948\u0964 \u092F\u0939 Base64 encoding \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0924\u093E \u0939\
  \u0948, \u091C\u094B \u0915\u093F \u092C\u0939\u0941\u0924\u2026"
lastmod: '2024-04-05T22:51:07.649005-06:00'
model: gpt-4-1106-preview
summary: "Basic authentication \u0915\u093E concept '90s \u0915\u0947 \u0936\u0941\
  \u0930\u0941\u0906\u0924\u0940 \u0926\u093F\u0928\u094B\u0902 \u0938\u0947 HTTP\
  \ protocol \u0915\u093E \u0939\u093F\u0938\u094D\u0938\u093E \u0939\u0948\u0964\
  \ \u092F\u0939 Base64 encoding \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\
  \u093E\u0932 \u0915\u0930\u0924\u093E \u0939\u0948, \u091C\u094B \u0915\u093F \u092C\
  \u0939\u0941\u0924 \u0938\u0941\u0930\u0915\u094D\u0937\u093F\u0924 \u0928\u0939\
  \u0940\u0902 \u0939\u0948, \u0914\u0930 \u0907\u0938\u0932\u093F\u090F HTTPS \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u091C\u0930\u0942\u0930\u0940 \u0939\u0948\
  \u0964 \u0907\u0938\u0915\u0947 \u0905\u0932\u093E\u0935\u093E, \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E\u0930 OAuth \u091C\u0948\u0938\u0947 \u0905\
  \u0927\u093F\u0915 \u0938\u0941\u0930\u0915\u094D\u0937\u093F\u0924 \u0924\u0930\
  \u0940\u0915\u094B\u0902 \u0915\u093E \u092D\u0940 \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 Basic authentication\
  \ \u0938\u093E\u0927\u093E\u0930\u0923 \u0914\u0930 \u091C\u0932\u094D\u0926\u0940\
  \ \u0938\u094D\u0925\u093E\u092A\u093F\u0924 \u0939\u094B\u0928\u0947 \u0915\u0947\
  \ \u0915\u093E\u0930\u0923 \u091B\u094B\u091F\u0947 applications \u092F\u093E \u0909\
  \u0928 \u092A\u0930\u093F\u092F\u094B\u091C\u0928\u093E\u0913\u0902 \u092E\u0947\
  \u0902 \u092A\u0938\u0902\u0926 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E\
  \ \u0939\u0948 \u091C\u0939\u093E\u0902 high-level security \u0915\u0940 \u0926\u0930\
  \u0915\u093E\u0930 \u0928\u0939\u0940\u0902 \u0939\u094B\u0924\u0940 \u0939\u0948\
  \u0964."
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

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

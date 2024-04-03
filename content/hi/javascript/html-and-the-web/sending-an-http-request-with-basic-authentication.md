---
date: 2024-01-20 18:01:51.536840-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: ."
lastmod: '2024-03-13T22:44:52.989098-06:00'
model: gpt-4-1106-preview
summary: .
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

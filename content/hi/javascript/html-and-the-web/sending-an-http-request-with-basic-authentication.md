---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
date:                  2024-01-20T18:01:51.536840-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

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

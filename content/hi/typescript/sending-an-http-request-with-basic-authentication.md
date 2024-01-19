---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध भेजना साथ में मौलिक सत्यापन का अर्थ है कि एक क्लाइंट एक सर्वर से कुछ जानकारी मांगता है, साथ ही एक प्रमाणीकरण हैडर भेजता है जिसे सर्वर बालीधान कर सके। प्रोग्रामर्स इसे तभी करते हैं जब वे एक HTTP अनुरोध के साथ सुरक्षित जानकारी भेजते हैं जैसे कि यूजरनेम और पासवर्ड। 

## कैसे: 

```TypeScript 
// हमें `http` और `Buffer` मॉड्यूल की आवश्यकता होती है: 
import { request } from 'http'; 
import { Buffer } from 'buffer'; 

// यूजरनेम और पासवर्ड का प्रयोग 
let username = 'यूजरनेम'; 
let password = 'पासवर्ड'; 

// हमें यूज़रनेम और पासवर्ड को Base64 में एन्कोड करना है
let authString = `${username}:${password}`;
let authEncoded = Buffer.from(authString).toString('base64'); 

// HTTP अनुरोध की स्थापना 
let options = { 
  host: 'example.com', 
  port: 80, 
  path: '/auth-endpoint', 
  method: 'GET', 
  headers: { 
    'Authorization': `Basic ${authEncoded}` 
  } 
}; 

// HTTP अनुरोध का प्रयोग 
let req = request(options, (res) => { 
  res.on('data', (data) => { 
    console.log(`Response: ${data.toString()}`); 
  }); 
}); 

req.end(); 
```

## गहराई में: 

**ऐतिहासिक प्रसंग:** मौलिक प्रमाणीकरण HTTP का एक बहुत पुराना भाग है, RFC 2617 (1999) में परिभाषित किया गया था। 

**विकल्प:** मौलिक प्रमाणीकरण कुछ विशेष स्थितियों में उपयोगी है, लेकिन आधुनिक एप्लिकेशन में आमतौर पर टोकन आधारित प्रमाणीकरण (जैसे JWT) का उपयोग किया जाता है। 

**कार्यान्वयन विवरण:** मौलिक प्रमाणीकरण विवरण को बेस64 में एन्कोड करता है, जो आसानी से डिकोड किया जा सकता है। इसलिए, इसका उपयोग केवल HTTPS संचार में ही किया जाना चाहिए।

## देखें भी: 

1. [HTTP प्रमाणीकरण (MDN)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
2. [TypeScript संदर्भ मैन्युअल](https://www.typescriptlang.org/docs/handbook/intro.html)
3. [Buffer वर्ग (Node.js डॉक्स)](https://nodejs.org/api/buffer.html)
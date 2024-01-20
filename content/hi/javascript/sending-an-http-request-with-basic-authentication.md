---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या & क्यों?

HTTP अनुरोध आधारित प्रमाणन के साथ भेजना एक प्रकार का तकनीक है जिससे सर्वर को यात्री डेटा की सुरक्षा और प्रामाणिकता की जांच हो सकती है। प्रोग्रामर्स इसे डेटा सुरक्षा सुनिश्चित करने और अनधिकृत पहुंच से बचने के लिए इस्तेमाल करते हैं।

## कैसे करें:

```JavaScript
// Node.js में 'axios' पैकेज का उपयोग करते हुए

const axios = require('axios');

axios({
  method: 'get',
  url: 'http://api_url',
  auth: {
    username: 'यूजरनेम',
    password: 'पासवर्ड'
  }
})
.then((response) => {
    console.log(response.data);
  })
.catch((error) => {
    console.error(error);
});
```

ऊपर दिए गए कोड स्निपेट का उपयोग करते हुए, HTTP अनुरोध के साथ बुनियादी प्रमाणीकरण डेटा भेज सकते हैं। 

## गहराई में:

1. ऐतिहासिक प्रसंग: HTTP बुनियादी प्रमाणन, 1996 में HTTP/1.0 मानक के हिस्सा के रूप में पेश किया गया था। इसे डाटा की एकल दिशाओं में सुरक्षा के लिए डिजाइन किया गया था।
2. विकल्प: HTTP Digest Authentication, OAuth, और SSL/TLS जैसी अन्य प्रामाणिकरण तकनीकें उपलब्ध हैं। ये सभी आपके व्यवस्थापक और आवश्यकताओं पर निर्भर करते हैं।
3. कार्यान्वयन विवरण: यदि आप अनुरोध पाठ के अंत में `Authorization` हेडर के साथ `Basic base64_encode(username:password)` जोड़ते हैं, तो HTTP बुनियादी प्रमाणीकरण लागू होता है।

## देखिए भी:

1. [MDN Web Docs: HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
2. [NPM: axios](https://www.npmjs.com/package/axios)
3. [Wikipedia: Basic access authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
---
title:                "बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
html_title:           "TypeScript: बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

हिंदी रीडर्स के लिए एक TypeScript प्रोग्रामिंग आर्टिकल:

## यह क्या है और क्यों? 
HTTP अनुरोध को आसान ऑथेंटिकेशन के साथ भेजना क्या है, और क्यों प्रोग्रामर्स इसे करते हैं, इसके बारे में दो से तीन सेटेंसेज़ बताती है। 

## कैसे करें:
```TypeScript
// बेसिक आसान ऑथेंटिकेशन के साथ HTTP अनुरोध भेजना
import axios from 'axios';
axios.get('http://example.com', {auth: {username: 'username', password: 'password'}})
    .then(response => console.log(response))
    .catch(error => console.log(error));
// संभावित आउटपुट: 200 अथवा 401 या 403
```

## गहराई में: 
HTTP अनुरोध को आसान ऑथेंटिकेशन के साथ भेजने के लिए अन्य विकल्पों के बारे में और प्रगति के साथ के बारे में विस्तृत जानकारी दी जाती है। 

1. ऐतिहासिक पृष्ठभूमि: आसान ऑथेंटिकेशन HTTP अनुरोध का उपयोग अतिरिक्त सुरक्षा स्तर प्रदान करने के लिए किया जाता है।
2. वैकल्पिक रूप से, OAuth और JWT जैसे अन्य प्रोटोकॉल इस्तेमाल किए जा सकते हैं जो बेहतर सुरक्षा प्रदान करते हैं।
3. इससे कैसे काम करता है: एक उदाहरण के साथ, एक यूजर आईडी और पासवर्ड को सर्वर को भेजकर, यह उपयोगकर्ता की पहचान के रूप में इस्तेमाल किया जाता है और एक टोकन जनरेट किया जाता है। यह टोकन अब से हर बार हेडर में शामिल करके, आसान ऑथेंटिकेशन के रूप में स्वीकृत हो जाता है। 

## देखें भी:
सम्बंधित स्रोतों के लिंक दिये गए हैं: 

- [आसान ऑथेंटिकेशन के साथ HTTP का अपनांदा उपयोग करना](https://www.moesif.com/blog/technical/authentication/Adding-Basic-Auth-to-Your-HTTP-APIs/)
- [आसान ऑथेंटिकेशन की विस्तृत जानकारी](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [HTTP कृपया आसानी सीखें](https://www.w3schools.com/js/js_ajax_http_send.asp)
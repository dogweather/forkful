---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP अनुरोध भेजना, कनेक्टेड सिस्टम्स में, एक विशेष वेब सर्वर से डाटा का आदान-प्रदान करने का तरीका होता है। कार्यक्रमकर्ताएं इसे वेब आधारित सर्विसेज इस्तेमाल करने, डेटा को मांगने या सर्वर पर कार्रवाई करने के लिए करते हैं।

## कैसे करें:
यहां एक साधारण TypeScript की HTTP अनुरोध भेजने की कोड संग्रहीत है:

```TypeScript
import axios from 'axios';

async function getData(url: string) {
  try {
    const response = await axios.get(url);
    console.log(response);
  } catch (error) {
    console.error(error);
  }
}

getData('https://api.example.com/data');
```

इस साधारण कोड में, हमने `axios` library का उपयोग करके HTTP GET अनुरोध भेजा और प्राप्त उत्तर को कन्सोल पर प्रिंट किया है।

## गहराई में जानना
HTTP अनुरोध का उपयोग डाटा के आदान-प्रदान में व्यापक तौर पर किया जाता है। इसने वेब पृष्ठों, REST एपीआईस और डाटा की अन्य सेवाओं के साथ इंटरैक्ट करने का मानक निर्धारित किया है।

वैकल्पिक तरीके जैसे कि WebSockets और Server Sent Events भी डाटा प्रेषण के लिए उपयोग हो सकते हैं, लेकिन ये HTTP अनुरोधों की तरह रूट नहीं होते हैं।

व्यावसायिक क्रम में, TypeScript के साथ HTTP अनुरोध का प्रबंधन अक्सर लाइब्रेरियों और फ्रेमवर्क्स जैसे कि `axios`, `fetch`, और `XHR` के माध्यम से किया जाता है।

## भी देखें
- [Axios निर्देशिका यहाँ](https://axios-http.com/)
- [Fetch API पूरी जानकारी](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [XMLHttpRequest का प्रलेखन](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
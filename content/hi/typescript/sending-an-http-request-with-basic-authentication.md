---
title:                "TypeScript: बेसिक प्रमाणीकरण के साथ एक HTTP अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक HTTP अनुरोध भेजना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

किसी भी वेब डेवलपमेंट परियोजना में जब हम सर्वर से डेटा लेने की जरूरत होती है तो हमें एक HTTP रिक्वेस्ट भेजने की आवश्यकता होती है। यह कार्य करने के लिए, हम बेसिक ऑथेंटिकेशन के साथ HTTP रिक्वेस्ट भेज सकते हैं जो एक सुरक्षित तरीके से सर्वर से डेटा को प्राप्त करता है। इस ब्लॉग पोस्ट में, हम जानेंगे कि बेसिक ऑथेंटिकेशन के साथ HTTP रिक्वेस्ट कैसे भेजा जाता है और इसके क्या फायदे हैं।

## कैसे

हमारे पास एक API को एक HTTP रिक्वेस्ट भेजने के लिए दो चीजें की आवश्यकता होती हैं - उपयोगकर्ता के नाम और पासवर्ड। हम यह जानने के लिए डेटा को डेटाबेस में स्टोर करते हैं या स्टेटिक कॉन्फिगरेशन फाइल में रखते हैं। HTTP रिक्वेस्ट प्रोसेस को बेहतर समझने के लिए, हम एक उदाहरण की मदद से यह देखेंगे।

```TypeScript
import axios from 'axios';

const username = "user";
const password = "pass";

axios.get('https://example.com/data', {
  auth: {
    username: username,
    password: password
  }
})
  .then(response => {
    console.log(response.data); // डेटा का उत्तर
  })
  .catch(error => {
    console.log(error);
  });
```
अपने अन्दर, हम `axios` पैकेज को इम्पोर्ट करते हैं जो HTTP रिक्वेस्ट भेजने के लिए बहुत उपयोगी हो सकता है। अपनी रिक्वेस्ट के साथ, हम `auth` ऑप्शन आधार में उपयोगकर्ता नाम और पासवर्ड भेजते हैं। यह एक सुरक्षित तरीके से हमारे रिक्वेस्ट को सर्वर तक पहुंचाता है।

## डीप डाइव

बेसिक ऑथेंटिकेशन ए
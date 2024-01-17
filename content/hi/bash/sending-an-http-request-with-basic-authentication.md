---
title:                "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
html_title:           "Bash: बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या होता है और क्यों?
बेसिक ऑथेंटिकेशन के साथ एचटीटीपी अनुरोध भेजने का मतलब है कि हम किसी सर्वर से डेटा को डाउनलोड करने के लिए अपने यूज़रनेम और पासवर्ड को सत्यापित करने के लिए अपना उपयोगकर्ता नाम और पासवर्ड के साथ एक अनुरोध भेजते हैं। इसे प्रोग्रामर्स अपने स्क्रिप्ट या ऐप्स में उपयोग करते हैं ताकि वे एक सुरक्षित और प्राइवेट तरीके से डेटा को डाउनलोड और सुपरिम्पोज़ कर सकें।

## कैसे:
*Bash ...* कोड ब्लॉक्स के भीतर प्रोग्रामिंग उदाहरण और नमूना आउटपुट

उदाहरण 1:
```Bash
curl -u username:password http://example.com/api/data
```

आउटपुट:
```
{"data": "This is the requested data"}
```

उदाहरण 2:
```Bash
wget --user=username --password=password http://example.com/api/data
```

आउटपुट:
```
{"data": "This is the requested data"}
```

## गहराई खोज:
*इतिहास प्रस्तावना, वैकल्पिक चरण और एचटीटीपी अनुरोध भेजने के लिए कार्यान्वयन विवरण*

* इतिहास प्रस्तावना: बेसिक ऑथेंटिकेशन हुआ पहले जब इंटरनेट का जन्म हुआ था और उस समय डाटा की सुरक्षा ज्यादा मुश्किल थी। इसके आविष्कार का श्रेय लिनस टोर्वाल्ड्स को दिया जाता है।
* वैकल्पिक चरण: आप एचटीटीपी के साथ बेसिक ऑथेंटिकेशन को उपयोग नहीं कर सकते हैं, आप अन्य जासूसी कोडिंग और अनुमतियों को भी उपयोग कर सकते हैं।
* एचटीटीपी अनुरोध कार्यान्वयन विवरण: कार्यान्वयन में, प्रोग्रामर अपने स्क्रिप्ट या ऐप में *curl* या *wget* कमांड का उपयोग करते हैं। उन्हें उपयोगकर्ता का नाम और पासवर्ड की आवश्यकता होती है ताकि वे अनुरोध को सफलतापूर्वक पूरा कर सकें।

## आगे की जाँच करें:
संबद्ध स्रोतों के लिंक

* [थंडरबर्ड API एचटीटीपी और बेसिक ऑथेंटिकेशन के साथ कम बात कैसे करें](https://developer.mozilla.org/en-US/docs/Mozilla/Projects/Thunderbird/API_Documentation/Thunderbird_API_With_Basic_Authenticated_HTTP)
* [यूज़रनेम और पासवर्ड के साथ प्रसिद्ध एचटीटीपी स्क्रिप्ट](https://superuser.com/questions/149329/username-and-password-for-basic-authentication-in-wget-script)
* [एचटीटीपी अनुरोध भेजने के लिए बेसिक ऑथेंटिकेशन का क्रॉस-भाषाई समर्थन](https://security.stackexchange.com/questions/
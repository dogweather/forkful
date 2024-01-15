---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

कई बार हमें अपने ऐप्लिकेशन को किसी सर्वर से डेटा को डाउनलोड या अपलोड करने की जरूरत होती है। यदि हम अपने सर्वर पर सुरक्षित तरीके से पहुंचना चाहते हैं, तो हमें अपने HTTP अनुरोध में बेसिक प्रमाणीकरण का उपयोग करना होगा। यह अनुरोध हमारे अनुरोध को सुरक्षित बनाता है और केवल सत्यापित उपयोगकर्ताओं को हमारे सर्वर से संवाद करने की अनुमति देता है।

## कैसे करें

अब हम HTTP अनुरोध में बेसिक प्रमाणीकरण का उपयोग करना सीखेंगे। निम्नलिखित उदाहरण में हम इस्तेमाल करेंगे: एक GET अनुरोध को उत्तरित करने के लिए एक API एंडपॉइंट का उपयोग करके जो एक उपयोगकर्ता की डेटा वापस लेता है। तो आइए शुरू करते हैं!

```C#
// आवश्यक पैकेज इंपोर्ट करें
using System.Net.Http;
using System.Net;

// `HttpClient` इंस्टेंस बनाने के लिए उपयोग किया जाता है।
HttpClient client = new HttpClient();

// निम्न लाइन भी हमें अपने क्रेडेंशियल को सेट करते हैं।
client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", Convert.ToBase64String(Encoding.UTF8.GetBytes("username:password")));

// अपने एपीआई एंडपॉइंट तक अपने अनुरोध को सेट करें
HttpResponseMessage response = await client.GetAsync("https://myapi.com/endpoint");

// उत्तर को उद्धृत करें
Console.WriteLine(response.StatusCode);
```

सूचना: ऊपर दिए गए उदाहरण में, हमने अनुमानित वैल्यूज को स्थापित नहीं किया है। आपको अपने खुद के उपयोगकर्ता नाम और पासवर्ड को प्रतिस्थापित करना होगा।

## डीप डाइव
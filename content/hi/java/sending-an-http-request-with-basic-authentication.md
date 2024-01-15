---
title:                "बेसिक प्रमाणीकरण के साथ एक HTTP अनुरोध भेजना"
html_title:           "Java: बेसिक प्रमाणीकरण के साथ एक HTTP अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक HTTP अनुरोध भेजना"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

बेसिक ऑथेंटिकेशन के साथ HTTP अनुरोध भेजने में क्यों लगें? स्वरप्रधान नेटवर्किंग और सुरक्षा उदाहरणों के रूप में यह प्रयुक्ति एक सरल तरीके से सुरक्षित डेटा संचार को सुनिश्चित करती है।

## कैसे करें

कोडिंग उदाहरणों और "```Java ...```" कोड ब्लॉक के भीतर उदाहरण आउटपुट। नीचे दिए गए कोड ब्लॉक में प्रदर्शित उदाहरण में, हम एक HTTP कनेक्शन के साथ बेसिक ऑथेंटिकेशन का उपयोग करते हुए सर्वर से डेटा प्राप्त करते हैं। 

```Java
// यूआरएल 
String url = "https://example.com/api/data";
// बेसिक ऑथेंटिकेशन की सामान्य जानकारी
String username = "username";
String password = "password";
// ऑथेंटिकेशन स्ट्रिंग बनाना
String authString = username + ":" + password;
// अनुरोध हेडर में ऑथेंटिकेशन जोड़ना
con.setRequestProperty("Authorization", "Basic " + Base64.getEncoder().encodeToString(authString.getBytes()));
// अनुरोध भेजना 
int responseCode = con.getResponseCode();
```

उपरोक्त उदाहरण में, हम एक यूआरएल सेट करते हैं जिससे हम डेटा प्राप्त करना चाहते हैं। फिर, हम एक सामान्य उपयोगकर्ता नाम और पासवर्ड सेट करते हैं और उससे एक ऑथेंटिकेशन स्ट्रिंग बनाते हैं। अंत में, हम उपरोक्त मेथड को कॉल करते हैं और सर्वर से सफलतापूर्वक डेटा को प्राप्त करते हैं। इसके अलावा, हम अपने कोड में ऑथेंटिकेशन को जोड़ने के लिए `con.setRequestProperty()` मेथड का उपयोग करते हैं।

## डीप डाइव

HTTP अनुरोध भेजने की तकनीक के साथ बेसिक ऑथेंट
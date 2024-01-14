---
title:                "C: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

शुरुआत में, हम यह जानना चाहते हैं कि क्यों हम HTTP अनुरोध भेजने में बेसिक प्रमाणीकरण का उपयोग करें। बेसिक प्रमाणीकरण का उपयोग करने से, हम सुरक्षित ढंग से डेटा को सर्वर के लिए भेज सकते हैं जो आमतौर पर उपयोगकर्ता नाम और पासवर्ड से सुरक्षित होता है।

## कैसे करें
अब हम एक आसान उदाहरण के साथ सी में HTTP अनुरोध भेजने का प्रक्रिया देखेंगे। सबसे पहले, हम `curl` को इनस्टॉल करेंगे ताकि हम अपने टर्मिनल में HTTP रिक्वेस्ट भेज सकें। तो चलिए शुरुआत करते हैं!

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
  CURL *curl;
  CURLcode res;

  // सर्वर के लिए URL और उपयोगकर्ता नाम
  char *url = "https://www.example.com";
  char *username = "username";

  // पासवर्ड को आधार रूप में कॉन्टेंस 64बिट ASCII में लिखें
  char *password = "password";

  // कोंफ़िगर परमीटर्स
  curl = curl_easy_init(); // स्थापना करें
  if(curl) {
    // अपडेट URL
    curl_easy_setopt(curl, CURLOPT_URL, url);
    
    // अपडेट HTTP प्रमाणीकरण प्रकोष्ठ
    curl_easy_setopt(curl, CURLOPT_USERPWD, username, password);
    
    // नतीजे प्राप्त करें
    res = curl_easy_perform(curl);
    
    // स्थापित समाप्त करें
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

जब आप यह कोड रन करेंगे, आपको `www.example.com` से संबंधित सामान्य पेज का उत्तर मिलेगा। आप अपने उपयोगकर्ता नाम और पासवर्ड को अपनी खुद की साइट पर अनुकूलित कर सकते हैं।

## गहरी खुदाई
अब हम गहराई से देखेंगे कि यह प्रक्रिया कैसे काम करती है। जब हम ऊपर दिए गए कोड का उपयोग करते हैं,
---
title:                "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना।"
html_title:           "C: बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना।"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना।"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

HTTP अनुरोध को बेसिक प्रमाणीकरण के साथ भेजने का एक कारण यह हो सकता है कि आप सुरक्षित तरीके से दूरस्थ सर्वर से डेटा अनुरोध करना चाहते हैं।

## कैसे करें

आप बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजने के लिए निम्न साधनों का उपयोग कर सकते हैं:

```c
#include <stdio.h>
#include <curl/curl.h>
 
/*
 * साइट से डेटा प्राप्त करना है और बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजना है
 */
int main(void)
{
  CURL *curl;
  CURLcode res;
 
  // साइट का उड़ीकरण करें
  curl = curl_easy_init();
  if(curl) {
    // यूआरएल को सेट करें
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");
 
    // स्टैटस कोड प्राप्त करने के लिए हैंडलर संबंधित भेजें
    curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");
 
    // अपडेट मेमरी हैंडलर बनाएँ
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
 
    // डेटा प्राप्त करने के लिए कॉलबैक फंक्शन सेट करें
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &chunk);
 
    /* एचटीटीपी अनुरोध भेजें */
    res = curl_easy_perform(curl);
 
    // हाशश बदलें पृष्ठ हटाएं
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
 
    /* हमेशा फ्री के लिए बदलें के लिए CURL ग्राहक हटाएं */
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

## गहराई में जाएं

यह कोड उदाहरण बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजने का एक सरल उदाहरण है। हालांकि, आप इसे समापन और प्रमाणीकरण के साथ अन्य प्रकार के HTTP अनुरोध भेजने के लिए संशोधित कर सकते हैं। आप भावना कर सकते हैं कि यह आपके एप्लिकेशन में अत्यधिक महत्वपूर्ण ह
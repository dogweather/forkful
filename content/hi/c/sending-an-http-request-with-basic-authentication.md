---
title:                "बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
html_title:           "C: बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# क्या और क्यों?

HTTP अनुरोध भेजकर मूल अनुमति के साथ programming करने का एक प्रमुख तरीका है। यह बिना प्रमुख अनुमति के अनुरोधों को संभव बनाता है जो सुरक्षित या गोपनीय जानकारी को एक वेब सर्विस से लैन्ड करते हैं। यह प्रयोक्ताओं को अपने ऐप्स और वेबसाइट में उपलब्ध सुविधाओं का उपयोग करने पर विश्वास दिलाता है। 

# कैसे करें:

```
#include <stdio.h>
#include <curl/curl.h>

int main()
{
  CURL *curl;
  CURLcode res;
  
  // यहां आपको अपने उपयोगकर्ता नाम और पासवर्ड के साथ नया HTTP अनुरोध बनाना होगा
  const char *username = "your_username";
  const char *password = "your_password";

  curl = curl_easy_init();
  if(curl) {
    // यहां आपको अपनी वेब सर्विस के URL को दर्ज करना होगा
    curl_easy_setopt(curl, CURLOPT_URL, "https://api.example.com/");
    // अनुमति को प्रदान करें
    curl_easy_setopt(curl, CURLOPT_USERPWD, username:password);
    // अनुरोध भेजें
    res = curl_easy_perform(curl);
    // अनुरोध के परिणाम को छापें
    printf("%ld\n", response_code);
    // सत्र समाप्त करें
    curl_easy_cleanup(curl);
  }
}
```

# गहराई तक जाइए:

- यह तकनीक और सुविधाओं को HTTP अनुरोध के साथ अनुमति देने का प्रथम तरीका है।
- कुछ एल्टर्नेटिव तरीके भी उपलब्ध हैं जो अनुप्रयोगों या सर्विसेज के रूप में अनुमतियों को अन्य से अलग कर सकते हैं।
- इस तकनीक को अपनी ऐप्स या वेबसाइटों में अन्य तकनीकों के साथ एक्सक्लूजिव रूप से उपयोग करें ताकि आपके उपयोगकर्ताओं को अवसर मिल सके कि वे आपके साथ सुरक्षित रूप से जुड़ सके। 

# देखें भी:

- [CURL website](https://curl.se/libcurl/c/http-auth-alt.html)
- [HTTP अनुरोधी प्रोटोकॉल की टोर एकड़ की अनुमति](https://community.torproject.org/hardware/routers/)
- [HTTPS क्या है और क्यों हमें इसका उपयोग करना चाहिए?](https://www.cloudflare.com/en-in/learning/ssl/what-is-https/)
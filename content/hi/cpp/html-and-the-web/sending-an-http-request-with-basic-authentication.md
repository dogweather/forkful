---
date: 2024-01-20 18:02:28.542694-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u0915\u0947 \u0938\u093E\u0925\
  \ \u092E\u0942\u0932 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\u0930\u0923\
  \ (Basic Authentication) \u0938\u0930\u094D\u0935\u0930 \u0915\u094B username \u0914\
  \u0930 password \u092D\u0947\u091C\u0915\u0930 \u0905\u0928\u0941\u0930\u094B\u0927\
  \ \u092A\u0930 \u092A\u0939\u0941\u0902\u091A \u0928\u093F\u092F\u0902\u0924\u094D\
  \u0930\u0923 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0924\u092C\
  \ \u0909\u092A\u092F\u094B\u0917\u2026"
lastmod: '2024-03-13T22:44:52.844033-06:00'
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u0915\u0947 \u0938\u093E\u0925\
  \ \u092E\u0942\u0932 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\u0930\u0923\
  \ (Basic Authentication) \u0938\u0930\u094D\u0935\u0930 \u0915\u094B username \u0914\
  \u0930 password \u092D\u0947\u091C\u0915\u0930 \u0905\u0928\u0941\u0930\u094B\u0927\
  \ \u092A\u0930 \u092A\u0939\u0941\u0902\u091A \u0928\u093F\u092F\u0902\u0924\u094D\
  \u0930\u0923 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0924\u092C\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u091C\
  \u092C \u090F\u0915 \u0938\u0941\u0930\u0915\u094D\u0937\u093F\u0924 API \u092F\u093E\
  \ \u0935\u0947\u092C \u0938\u0947\u0935\u093E \u0938\u0947 \u0921\u0947\u091F\u093E\
  \ \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u093E \u0939\u094B\
  \u0924\u093E \u0939\u0948\u0964."
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

## कैसे करें:
```C++
#include <iostream>
#include <curl/curl.h>
#include <string>

// यह callback function है जो libcurl से डेटा को पढ़ता है
static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    // libcurl का उपयोग करके HTTP GET request करते हुए basic authentication के साथ
    CURL *curl = curl_easy_init();
    std::string readBuffer;
    if(curl) {
        // आपका यूआरएल और आपके credentials यहाँ जायेंगे
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/api/data");
        curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");
        
        // SSL verification को disable करना, डेवलपमेंट के दौरान उपयोगी
        curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
        curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 0L);
        
        // Define our write function
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        
        // Results को एक string में save करें
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        
        // अनुरोध को परफॉर्म करें और चेक करें के कोई एरर तो नहीं
        CURLcode res = curl_easy_perform(curl);
        if (res != CURLE_OK) {
            std::cerr << "curl_easy_perform() failed: " << curl_easy_strerror(res) << std::endl;
        } else {
            // सफलतापूर्वक प्राप्त डेटा को प्रिंट करें
            std::cout << readBuffer << std::endl;
        }
        // Cleanup
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

ध्यान दें कि इस कोड को चलाने के लिए आपको `libcurl` लाइब्रेरी install करनी होगी।

## गहराई से समझें:
HTTP Basic Authentication सबसे सरल HTTP authentication स्कीम में से एक है, जिसे RFC7617 में परिभाषित किया गया है। Username और password Base64 encoding प्रारूप में 'Authorization' हेडर के ज़रिये भेजे जाते हैं। यह सादगीपूर्ण होने के कारण अक्सर पहली पसंद रहती है, लेकिन कम सुरक्षित भी होती है क्योंकि Base64 encoding आसानी से decode की जा सकती है। जब सुरक्षा महत्वपूर्ण हो तब OAuth जैसी अधिक जटिल प्रमाणीकरण पद्धतियों का चयन करें।

इसीलिए जब Basic Authentication का उपयोग किया जाता है, तो यह HTTPS के साथ होना चाहिए ताकि क्रेडेंशियल्स encrypt हों और इंटरसेप्ट न हो सकें।

`libcurl` का use करते हुए हम आसानी से HTTP या HTTPS अनुरोध कर सकते हैं और Basic Authentication का प्रबंध भी कर सकते हैं। यह cross-platform है और विविध HTTP कार्यक्षमताओं का समर्थन करता है।

## संबंधित सूत्र:
- `libcurl` के बारे में और जानने के लिए: [libcurl - the multiprotocol file transfer library](https://curl.haxx.se/libcurl/)
- RFC 7617, The 'Basic' HTTP Authentication Scheme: [RFC 7617](https://tools.ietf.org/html/rfc7617)
- C++ कोड के examples और tutorials के लिए: [C++ Documentation](https://en.cppreference.com/w/)

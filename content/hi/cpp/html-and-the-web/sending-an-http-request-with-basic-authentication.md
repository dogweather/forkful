---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
aliases: - /hi/cpp/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:28.542694-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP अनुरोध के साथ मूल प्रमाणीकरण (Basic Authentication) सर्वर को username और password भेजकर अनुरोध पर पहुंच नियंत्रण करता है। प्रोग्रामर्स इसे तब उपयोग करते हैं जब एक सुरक्षित API या वेब सेवा से डेटा प्राप्त करना होता है।

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

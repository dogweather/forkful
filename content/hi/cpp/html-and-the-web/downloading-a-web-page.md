---
title:                "वेब पेज डाउनलोड करना"
aliases:
- /hi/cpp/downloading-a-web-page.md
date:                  2024-01-20T17:43:48.184494-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
वेब पेज डाउनलोड करना मतलब इंटरनेट से सूचना लाना और उसे अपने कंप्यूटर पर सेव करना। प्रोग्रामर्स यह काम ऑटोमेटिक डेटा कलेक्शन, टेस्टिंग वेब ऐप्लिकेशन्स या वेब सर्वर्स की मॉनिटरिंग के लिए करते हैं।

## कैसे करे? (How to)
C++ में वेब पेज डाउनलोड करने के लिए, हमें एक थर्ड-पार्टी लाइब्रेरी जैसे कि `CURL` की जरूरत होती है। `libcurl` एक मुफ्त, ओपन-सोर्स लाइब्रेरी है जो डेटा ट्रांसफर के लिए इस्तेमाल होती है।

```cpp
#include <iostream>
#include <curl/curl.h>

size_t callback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL *curl;
    CURLcode res;
    std::string response_data;

    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, callback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_data);
        
        res = curl_easy_perform(curl);
        if(CURLE_OK == res) {
            std::cout << response_data;
        } else {
            std::cerr << "CURL failed: " << curl_easy_strerror(res) << std::endl;
        }

        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();

    return 0;
}
```

यह कोड उदाहरण `http://example.com` वेब पेज को डाउनलोड करके टर्मिनल में प्रिन्ट कर देता है।

## गहराई से जानकारी (Deep Dive)
वेब पेजेज को डाउनलोड करने के तरीके काफी समय से विकसित हो रहे हैं। पुराने जमाने में `telnet` या फिर कमांड-लाइन टूल `wget` का इस्तेमाल होता था। `CURL` का प्रयोग सबसे आम है, लेकिन अन्य विकल्प जैसे कि `http/https` क्लाइंट्स होते हैं।

`CURL` की मदद से HTTP गेट, पोस्ट, और मल्टीपार्ट फॉर्म डेटा रिक्वेस्ट्स को आसानी से बनाया जा सकता है। यह विभिन्न प्रोटोकॉल्स को सपोर्ट करता है और SSL/TLS एन्क्रिप्शन के साथ सुरक्षित कनेक्शन की सुविधा देता है। इसके एपीआई सेट को समझना और लागू करना रिलेटिवली सरल है।

`libcurl` कई प्लेटफार्म्स पर उपलब्ध है, और इसे सीधे सिस्टम के पैकेज मैनेजर (जैसे कि `apt` या `brew`) से इंस्टॉल किया जा सकता है। 

## और भी जानकारी के लिए (See Also)
- CURL Official Website: [https://curl.haxx.se/](https://curl.haxx.se/)
- libcurl Programming Tutorial: [https://curl.haxx.se/libcurl/c/libcurl-tutorial.html](https://curl.haxx.se/libcurl/c/libcurl-tutorial.html)
- cURL Command Line Usage: [https://curl.haxx.se/docs/manpage.html](https://curl.haxx.se/docs/manpage.html)
- C++ Network Programming with Boost.Asio: [https://www.boost.org/doc/libs/1_65_1/doc/html/boost_asio.html](https://www.boost.org/doc/libs/1_65_1/doc/html/boost_asio.html)

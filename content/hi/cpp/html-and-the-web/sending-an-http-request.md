---
date: 2024-01-20 17:59:23.219920-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): Sample Output."
lastmod: '2024-04-05T21:53:54.801336-06:00'
model: gpt-4-1106-preview
summary: ''
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

## How to (कैसे करें):
```C++
#include <iostream>
#include <curl/curl.h> // libcurl का उपयोग

// Response data को संग्रहित करने के लिए callback function
size_t callbackFunction(char* ptr, size_t size, size_t nmemb, std::string* data) {
    data->append(ptr, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL* curl; // CURL object को परिभाषित करना
    CURLcode res;
    std::string response_data; // Response को संग्रहित करने के लिए

    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init(); // CURL easy session को इनिशिएट करना

    if (curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com"); // URL सेट करना
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, callbackFunction); // Write callback सेट करना
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_data); // Response data के लिए वेरिएबल सेट करना

        res = curl_easy_perform(curl); // Request को प्रदर्शित करना

        if (res != CURLE_OK) { // Error checking
            std::cerr << "CURL failed: " << curl_easy_strerror(res) << std::endl;
        } else {
            std::cout << "Response Data: " << response_data << std::endl; // Output
        }

        curl_easy_cleanup(curl); // Cleanup
    }

    curl_global_cleanup();
    return 0;
}
```

Sample Output:
```
Response Data: <html>...
...
</html>
```

## Deep Dive (गहराई से जानकारी):
सन 1990 में HTTP प्रोटोकॉल को मूल रूप से विकसित किया गया था, जिससे वेबसाइट्स के बीच संचार सरल हो गया। समय के साथ, यह ऑनलाइन संसार की रीढ़ बन गया। विकल्पों में gRPC, WebSocket आदि शामिल हैं, जो विशेष प्रयोजनों के लिए हैं।

HTTP अनुरोध भेजने के लिए C++ में सबसे लोकप्रिय लाइब्रेरी libcurl है। यह व्यापक, सुरक्षित है और मल्टीपल प्लेटफार्म्स पर काम करती है। इसमें सिंक्रोनस और असिंक्रोनस दोनों तरह के अनुरोधों को संधारित करने की क्षमता होती है।

## See Also (सम्बंधित सूत्र):
- [libcurl](https://curl.se/libcurl/)
- [HTTP ऑफिशियल डॉक्युमेंटेशन](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [Curl कमांड लाइन टूल का उपयोग](https://curl.se/docs/manpage.html)

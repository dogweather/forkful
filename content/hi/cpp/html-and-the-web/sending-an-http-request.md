---
title:                "HTTP अनुरोध भेजना"
aliases:
- hi/cpp/sending-an-http-request.md
date:                  2024-01-20T17:59:23.219920-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

HTTP अनुरोध (HTTP Request) भेजना इंटरनेट पर सर्वर से डाटा मांगने की प्रक्रिया है। प्रोग्रामर्स इसे वेबसाइट्स से जानकारी प्राप्त करने, API से डाटा मैनेज करने और दूरस्थ सेवाओं के संग इंटरैक्ट करने के लिए करते हैं।

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

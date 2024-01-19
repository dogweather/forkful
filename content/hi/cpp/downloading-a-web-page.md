---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

(What & Why?)

वेब पेज डाउनलोड करना मतलब किसी स्पेसिफिक URL से डाटा लेना. प्रोग्रामर्स इसे वेब कन्टेंट की स्क्रेपिंग, डाटा एनालिसिस, अथवा ऑटोमेटिड टेस्टिंग के लिए करते हैं।

## कैसे:

(How To:)

यहाँ एक C++ कोड दर्ज है जो [Curl](https://curl.haxx.se/libcurl/c/) लाइब्ररी का इस्तेमाल करके वेब पेज डाउनलोड करता है:

```C++
#define CURL_STATICLIB
#include <curl/curl.h>
   
size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* s) {
    size_t newLength = size*nmemb;
    s->append((char*)contents, newLength);
    return newLength;
}

int main() {
    CURL* curl;
    CURLcode res;
    std::string s;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &s);

        res = curl_easy_perform(curl);

        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        }
        curl_easy_cleanup(curl);
    }
    curl_global_cleanup();
    return 0;
}
```
यह कोड `http://example.com` वेबपेज की सामग्री लोड करता है और उसे string `s` में सहेजता है। यदि कोई त्रुटि होती है, तो यह त्रुटि मैसेज दिखाता है।

## गहराई में :

(Deep Dive:)

ये कॉड एक C++ लाइब्ररी, Curl, का उपयोग करता है । Curl एक मुफ्त और open-source लाइब्ररी है, जिसे पहली बार १९९७ में जारी किया गया था।  इसके विकल्प में POCO और Beast जैसे लाइब्रेरियाँ हैं|

## यदि देखना चाहें :

(See Also:)

- Curl C++ के लिए डाक्यूमेंटेशन [Curl C++ Documentation](https://curl.haxx.se/libcurl/c/)
- SO सवाल - "How to Download a Webpage in C++" [StackOverflow Question](https://stackoverflow.com/questions/6292332/what-really-is-a-curl-in-terms-of-programming)
- वेब स्क्रेपिंग के बारे में अधिक: [Web Scraping](https://en.wikipedia.org/wiki/Web_scraping)
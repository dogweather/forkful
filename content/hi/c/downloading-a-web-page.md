---
title:                "वेब पेज डाउनलोड करना"
date:                  2024-01-20T17:43:37.203285-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वेब पेज डाउनलोड करने का अर्थ है इंटरनेट से वेब पेज की सामग्री प्राप्त करना। प्रोग्रामर इसे डेटा एक्सट्रैक्शन, वेबसाइट्स के बैकअप या ऑफ़लाइन एनालिसिस के लिए करते हैं।

## How to: (कैसे करें:)
```C
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "downloaded_page.html";
    
    curl = curl_easy_init();
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
        fclose(fp);
    }
    return 0;
}
```

यह प्रोग्राम "http://example.com" वेब पेज को "downloaded_page.html" फाइल में डाउनलोड करेगा। आउटपुट फाइल में वेब पेज का HTML कोड होगा।

## Deep Dive (गहराई से जानकारी):
वेब पेज को डाउनलोड करना इंटरनेट के शुरुआती दिनों से हो रहा है। पहले लोग `telnet` या `ftp` का उपयोग करते थे, मगर अब ‘CURL’ लाइब्रेरी पसंदीदा उपकरण है। CURL का इस्तेमाल करके, हम आसानी से डेटा ट्रांसफर कर सकते हैं। 

अन्य विकल्प में 'wget' कमांड लाइन प्रोग्राम भी शामिल है जो यही काम करता है। 

इम्प्लीमेंटेशन में हमेशा `libcurl` लाइब्रेरी की ज़रूरत होती है। `write_data` फंक्शन डाउनलोडेड डेटा को फ़ाइल में लिखता है, और `curl_easy_init`, `curl_easy_setopt`, `curl_easy_perform` फंक्शन्स डाउनलोड करने के लिए cURL सेशन को शुरू और संचालित करते हैं।

## See Also (और जानें):
- cURL प्रोजेक्ट की वेबसाइट: [Curl Official Website](https://curl.se/)
- libcurl प्रोग्रामिंग का ट्यूटोरियल: [libcurl Tutorial](https://curl.se/libcurl/c/libcurl-tutorial.html)
- 'wget' कमांड पर जानकारी: [GNU Wget](https://www.gnu.org/software/wget/)
- HTTP प्रोटोकॉल पर विस्तार से जानकारी: [HTTP Protocol](https://developer.mozilla.org/en-US/docs/Web/HTTP)
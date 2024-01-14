---
title:                "C: एक वेब पेज को डाउनलोड करना"
simple_title:         "एक वेब पेज को डाउनलोड करना"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों

एक वेब पेज को डाउनलोड करने का काम आसान है और सही तरीके से कोडिंग करेंगे तो यह भी काफी आसान लगेगा।

## कैसे करें

```C
// आवश्यक हैडर फाइल, इसमें वेब पेज के डाउनलोड के लिए फंक्शन होंगे
#include <stdio.h>
#include <curl/curl.h>

int main() {
  // वेब पेज के डाउनलोड के लिए कोड
  CURL *curl;
  CURLcode res;

  // वेब पृष्ठ को डाउनलोड करने के लिए URL सेट करें
  curl = curl_easy_init();
  if (curl) {
    // डाउनलोड करने के लिए URL सेट करें
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/");

    // डाउनलोड किया गया डेटा सहेजें
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);

    // कोड का नतीजा प्राप्त करें
    res = curl_easy_perform(curl);

    /* curl को क्लीनअप करें */
    curl_easy_cleanup(curl);
  }

  return 0;
}

/** परिभाषित write_data फंक्शन को वेब पृष्ठ से डाउनलोड किया गया डेटा को सहेजने के लिए उपयोग किया जाता है */
size_t write_data(void *buffer, size_t size, size_t nmemb, void *userp) {
  // वेब पेज से डाउनलोड किए गए डेटा को स्टैंडर्ड आउटपुट पर प्रिंट करें
  printf("%s", (char *)buffer);

  return size * nmemb;
}
```

## गहराई में जाइए

वेब पृष्ठ को डाउनलोड करने के कोड में हमने `curl_easy_setopt()` फंक्शन का उपयोग किया है जो `CURLOPT_URL` पैरामीटर के माध्यम से दिए गए URL पर कनेक्शन बनाता है। हमने भी `CURLOPT_WRITEFUNCTION` पैरामीटर का उपयोग किया है जो डाउनलोड किए गए डेटा को हैंडल करता है और हमारे फंक्शन `write_data()` को कॉल करता है जो डाउनलोड किया गया डेटा को स्टैंडर्ड आउटपुट पर प्रिंट करता है।

## देखें भी

"वेब पृष
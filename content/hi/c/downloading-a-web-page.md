---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## व्हाट एंड वाय? [What & Why?]

वेब पेज डाउनलोड करना क्या होता है और क्यों प्रोग्रामर्स इसे करते हैं?
वेब पेज को डाउनलोड करना मतलब उसे इंटरनेट से अपने कंप्यूटर पर कॉपी करना। प्रोग्रामर्स इसे करते हैं ताकि उन्हें वेबसाइट की जानकारी और डाटा को प्रोसेस करने के लिए लोकली उपलब्ध हो सके। 

## कैसे [How to:]

C लैंग्वेज में, आप libcurl का उपयोग करके वेब पेज को डाउनलोड कर सकते हैं। यहाँ एक सादा उदाहरण है:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

    /* if redirect is needed */
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);

    res = curl_easy_perform(curl);

    /* error handling */
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    /* cleanup */
    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();

  return 0;
}
```

जब आप इस कोड को चलाते हैं, तो example.com का होमपेज डाउनलोड होता है। 

## डीप डाइव [Deep Dive]

वेब पेज को डाउनलोड करने की सोच को HTTP (HyperText Transfer Protocol) के अविष्कार के साथ ही जन्म दिया गया था। इसके विकल्प में वेब स्क्रेपिंग और API (Application Programming Interface) कॉल्स शामिल हैं।

C प्रोग्रामिंग का उपयोग करके वेब पेज डाउनलोड करने का कार्य libcurl बिल्बोर्धीयन, नेटवर्क-अधारित कार्यों को आसान करने के लिए तैयार की गई एक खुली स्रोत पुस्तकालय के माध्यम से होता है। 

## देखने के लिए यह देखें [See Also]

1. [CURL का डॉक्युमेंटेशन](https://curl.haxx.se/libcurl/c/) 
2. [वेब स्क्रेपिंग ट्यूटोरियल](https://www.datacamp.com/community/tutorials/web-scraping-using-python)
3. [HTTP प्रोटोकॉल का इतिहास](https://www.w3.org/History.html)
---
title:                "HTTP अनुरोध भेजना"
html_title:           "C: HTTP अनुरोध भेजना"
simple_title:         "HTTP अनुरोध भेजना"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP अनुरोध भेजना एक प्रोग्राम द्वारा किया जाने वाला काम है। इसका सबसे बड़ा कारण है कि प्रोग्रामर विभिन्न वेबसाइटों और सर्वरों से डेटा को प्राप्त करना और उसे अपने प्रोग्राम में शामिल करना चाहते हैं। 

## कैसे करें?
यहां हम एक GET अनुरोध भेजकर GitHub वेबसाइट से डेटा प्राप्त करते हैं: 
```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.github.com");
    res = curl_easy_perform(curl);

    /* always cleanup */
    curl_easy_cleanup(curl);
  }
  return 0;
}
```
उपरोक्त कोड का परिणाम यह होगा:
```C
<!doctype html>
<html class="no-js" lang="en">
  <head>
    <meta charset="utf-8">
  </head>
  <body>
  ...
```

## गहराई में जाएँ
1. HTTP का इतिहास: HTTP एक प्रसिद्ध इंटरनेट प्रोटोकॉल है जिसका उपयोग वेबसाइटों के बीच डेटा को शेयर करने के लिए होता है। यह कंप्यूटर नेटवर्क्स की तकनीकी मानक है जो अपने उपयोगकर्ताओं को इंटरनेट से डेटा अनुरोध करने की अनुमति देता है। 
2. अल्टरनेटिव: कुछ अन्य चीजें HTTP अनुरोधों के लिए उपयोगी हो सकती हैं, जैसे Libcurl या cURL सहित विभिन्न लाइब्रेरी या टूल्स। 
3. अनुमानित समय: HTTP अनुरोध भेजने के लिए इस्तेमाल की जाने वाली डाटा के आकार और अन्य तकनीकी विवरणों की गहराई।

## और भी देखें
- https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview
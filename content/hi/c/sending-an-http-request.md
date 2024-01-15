---
title:                "एक HTTP अनुरोध भेजना"
html_title:           "C: एक HTTP अनुरोध भेजना"
simple_title:         "एक HTTP अनुरोध भेजना"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

आपने निश्चित रूप से इंटरनेट पर सर्वसाधारण चीजों को ब्राउज़ किया है। आपने यह सोचा होगा कि सभी वेबसाइट पर जो सुविधाएं हैं, उसे कैसे प्राप्त किया जाता है। 

## कैसे करें

```C
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/");
    res = curl_easy_perform(curl);
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

यहां हम CURL का उपयोग करके HTTP अनुरोध भेजने का एक उदाहरण देख सकते हैं। इसमें हम पूरी जानकारी को फफोहराते नहीं हैं, लेकिन फिर भी सीखोगे। 

## गहराई में

एचटीटीपी अनुरोध भेजना असाधारण ढंग से महत्वपूर्ण है। यह सबकुछ शुरू होता है जब आप एक वेबसाइट या सेवा के लिए फ़ैक्चर या डेटा काम कराते हैं। प्राथमिक ढंग से, आप वेबसाइट पते को एक सामान्य ब्राउज़र के माध्यम से दर्ज करते हैं। इससे आपका अनुरोध नैतिक रूप से वेबसाइट के सर्वर तक पहुँचता है और उससे योग्य उत्तर भेजा जाता है। इस प्रक्रिया को एचटीटीपी अनुरोध भेजने कहते हैं। 

## देखें भी

- [हर चीज़ जो आपने कभी एचटीटीपी के बारे में पूछा है](https://www.tutorialspoint.com/http/http_quick_guide.htm)
- [एचटीटीपी से जुड़ी शब्दावली](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)
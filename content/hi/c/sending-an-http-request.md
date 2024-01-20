---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## व्हाट एंड वाई (What & Why)?

HTTP अनुरोध (निवेदन) भेजना वेब सर्वर को किसी विचरण के लिए निर्देश प्रदान करने का एक तरीका है। प्रोग्रामर्स इसे वेब सर्विसेज, APIस और वेबसाइट्स से डेटा इंटरेक्ट करने के लिए करते हैं। 

## कैसे करें (How to):

HTTP अनुरोध को `libcurl` बिल्ज़ोर्डी (library) के साथ निम्न प्रोग्राम में उदाहरण देखें:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
  
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

## डीप डाइव (Deep Dive):

HTTP अनुरोध को 90 के दशक में HTTP प्रोटोकॉल के साथ डिज़ाइन किया गया था। इसे नामांकन (naming), निर्देशन (directing) और उत्पादकता (productivity) के लिए बनाया गया था। 

विकल्पों में, आप जवास्क्रिप्ट के `fetch` फ़ंक्शन या पायथन के 'requests' मॉड्यूल का उपयोग कर सकते हैं। 

C में HTTP अनुरोध को परिष्कृत करने में लाइब्रेरी जैसे कि `libcurl` आमतौर पर उपयोग की जाती हैं, क्योंकि निवेदन बनाने और प्राप्त करने का लोजिक संभावित रूप से जटिल हो सकता है। 

## अधिक देखें (See Also):

1. [libcurl documentation](https://curl.se/libcurl/c/)
2. [HTTP protocol RFC](https://tools.ietf.org/html/rfc2616)
3. [Python requests library for HTTP requests](https://docs.python-requests.org/en/master/)
4. [Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
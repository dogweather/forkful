---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

# क्या और क्यों? (What & Why?)
HTTP अनुरोध भेजना मतलब किसी वेबसर्वर से डाटा मांगना। प्रोग्रामर्स इसे वेबसर्वर से विभिन्न प्रकार की जानकारी हासिल करने के लिए करते हैं। 

# कैसे करें: (How to:) 
C++ में, आप एक अनुरोध भेजने के लिए 'cURL' लाइब्रेरी का उपयोग कर सकते हैं। 

```C++
#include <curl/curl.h>

int main() {
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);
  curl = curl_easy_init();

  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
    res = curl_easy_perform(curl);
    if(res != CURLE_OK) {
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    }
    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();

  return 0;
}
```
इस कोड में, आप "http://example.com" पर HTTP अनुरोध भेज रहे हैं। अगर कुछ भी गलत हुआ है, तो आपको उसकी जानकारी मिलेगी। 

# गहरी जानकारी: (Deep Dive)
HTTP अनुरोधों का इस्तेमाल वेब की विकास प्रक्रिया में महत्वपूर्ण भूमिका निभा रहा है। cURL के विकल्प के रूप में, आप `WinINet`, `libwww`, `Poco` आदि का उपयोग कर सकते हैं। cURL बहु भाषीय लाइब्रेरी है जिसे C, C++, Python आदि में उपयोग किया जा सकता है। 

# एक्स्ट्रा जानकारी: (See Also)
अधिक जानकारी के लिए, आप निम्नलिखित लिंक पर जा सकते हैं:
- cURL कार्यान्वयन: [curl tutorial](https://ec.haxx.se/)
- HTTP अनुरोध के विविध प्रकार: [types of HTTP requests](https://www.restapitutorial.com/)
- अन्य C++ लाइब्रेरी: [other C++ libraries](https://www.slant.co/topics/7890/~best-http-clients-for-c-plus-plus)
---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

HTTP अनुरोध के साथ Basic Authentication (मूल प्रमाणीकरण) एक संवाददाता के रूप में किसी सर्वर को पहचानने का तरीका है। प्रोग्रामर्स यह सुरक्षित और विश्वसनीय डेटा के अदल-बदल के लिए करते हैं।

## How to: (कैसे करें)

```C 
#include <curl/curl.h>

int main(void){

  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");

    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();

  return 0;
}
```
इस कोड का आउटपुट कुछ ऐसा होगा:
```
curl_easy_perform() failed: Failed to connect to example.com port 443: Connection refused
```

## Deep Dive (गहरी जांच)

### Historical Context (ऐतिहासिक संदर्भ)
Basic Authentication HTTP प्रमाणीकरण की सबसे सरल प्रकार है और यह HTTP 1.0 ज़माने से है। 

### Alternatives (विकल्प)
OAuth और Digest Access Authentication, Basic Authentication के मुकाबले अधिक सुरक्षित विकल्प हो सकते हैं। 

### Implementation Details (कार्यान्वयन विवरण)
CURL लाइब्रेरी, जो HTTP अनुरोधों को करने के लिए बहुत लोकप्रिय है। यह प्रमाणन और सत्यापन को संभालता है।

## See Also (यह भी देखें):

1. **libcurl**: (https://curl.haxx.se/libcurl/c/) libcurl का API डॉक्यूमेंटेशन। 
2. **HTTP प्रमाणीकरण**: (https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication) MDN Web Docs में HTTP प्रमाणीकरण। 
3. **CURL प्रलेखन**: (https://curl.haxx.se/docs/) CURL स्थापना और उपयोग के निर्देश।

ध्यान दें कि दी गई URL अस्थायी हो सकती हैं।
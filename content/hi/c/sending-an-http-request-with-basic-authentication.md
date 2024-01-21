---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
date:                  2024-01-20T18:01:01.941259-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP request भेजना वेब सर्वर पर data मांगना या भेजना है। Basic authentication वह process है जिसमें username और password से हम अपनी पहचान साबित करते हैं। यह जानकारी को सुरक्षित रखते हुए सर्वर से संवाद करने के लिए की जाती है।

## कैसे करें:

```C
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int main() {
    CURL *curl = curl_easy_init();
    if(curl) {
        struct curl_slist *headers = NULL;
        headers = curl_slist_append(headers, "Content-Type: application/json");

        // Username and password for basic authentication
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "your-username");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "your-password");

        // Setting the URL for the request
        curl_easy_setopt(curl, CURLOPT_URL, "https://api.example.com/data");
        
        // Execute the HTTP request
        CURLcode res = curl_easy_perform(curl);

        if(res != CURLE_OK) {
            fprintf(stderr, "Curl request failed: %s\n", curl_easy_strerror(res));
        }

        // Cleanup
        curl_slist_free_all(headers);
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Sample output के लिए यहाँ नहीं कुछ दिखाया जा सकता है क्योंकि यह program काम करने पर कोई विशिष्ट output terminal पर नहीं दिखाता। CURL library का use करके हम HTTP request भेजते हैं और यदि कोई error हो तो उसे terminal पर print करते हैं। 

## गहराई से जानकारी:

HTTP Basic Authentication इंटरनेट के शुरुआती दिनों से है। यह Base64 encoding का उपयोग करता है लेकिन एन्क्रिप्टेड नहीं होता, इसलिए HTTPS के साथ use करना ज्यादा सुरक्षित है।

Alternatives में OAuth, Digest Authentication और API keys शामिल हैं, जो ज्यादा सुरक्षा और features देते हैं।

इम्प्लीमेंटेशन के लिए, C language में अकसर libcurl library का इस्तेमाल होता है, जो एक मजबूत और versatile tool है। यह multiple protocols support करता है और platform-independent है। इसका मतलब है कि एक बार सीखने पर आप इसे विभिन्न प्रकार की projects में use कर सकते हैं।

## और देखें:

- libcurl का official documentation: [curl.haxx.se/libcurl](https://curl.haxx.se/libcurl/)
- HTTP Basic Authentication के बारे में MDN Web Docs: [developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- Basic Authentication के alternatives के लिए OAuth 2.0: [oauth.net/2/](https://oauth.net/2/)
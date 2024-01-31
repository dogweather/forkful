---
title:                "HTTP अनुरोध भेजना"
date:                  2024-01-20T17:59:21.710260-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTTP अनुरोध भेजना एक वेब सर्वर से डेटा मांगने की प्रक्रिया है। प्रोग्रामर यह कार्य वेब सेवाओं का उपयोग करके जानकारी प्राप्त करने या डेटा भेजने के लिए करते हैं।

## How to: (कैसे करें?)
सी में HTTP अनुरोध भेजने के लिए, आप libcurl का उपयोग कर सकते हैं। इसे पहले स्थापित करना पड़ सकता है। नीचे साधारण GET अनुरोध का कोड है:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        
        // अनुरोध पूरा होने पर उत्तर दिखाने के लिए
        curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
        
        // HTTP GET करने के लिए
        res = curl_easy_perform(curl);
        
        // सफलता या त्रुटि की जांच
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

        // समाप्ति
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

संभावित आउटपुट:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive (गहराई से जानकारी)
प्रारंभिक इंटरनेट दिनों में, सी से HTTP अनुरोध भेजना जटिल था। लेकिन लाइब्रेरियां जैसे कि libcurl ने इसे सरल बना दिया है। libcurl बहुत सारे प्रोटोकॉल जैसे HTTP, HTTPS, FTP, आदि का समर्थन करती है। 

विकल्प के रूप में, आप sockets का उपयोग करके निचले स्तर पर HTTP अनुरोध भेज सकते हैं। लेकिन libcurl जैसे उच्च स्तरीय एब्स्ट्रैक्शन प्रदान करने वाली लाइब्रेरियां इस काम को आसान बनाती हैं।

एक HTTP अनुरोध में आमतौर पर एक रिक्वेस्ट लाइन, हेडर्स और वैकल्पिक बॉडी होती है। GET, POST, PUT, DELETE आदि विभिन्न प्रकार के HTTP मेथड्स हैं जिन्हें आप भेज सकते हैं।

## See Also (यह भी देखें)
- cURL Official Documentation: https://curl.haxx.se/libcurl/c/
- HTTP on Wikipedia: https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol
- RFC 7230 (HTTP/1.1): https://tools.ietf.org/html/rfc7230

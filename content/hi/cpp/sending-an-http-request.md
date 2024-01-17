---
title:                "एक एचटीटीपी अनुरोध भेजना"
html_title:           "C++: एक एचटीटीपी अनुरोध भेजना"
simple_title:         "एक एचटीटीपी अनुरोध भेजना"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध भेजना एक कंप्यूटर के साथ अन्य कंप्यूटर के बीच डेटा को एक जगह से दूसरी जगह भेजने की प्रक्रिया है। प्रोग्रामर इसे डेटा को शेयर करने, संचार करने या सर्वर से डेटा प्राप्त करने के लिए करते हैं।

## कैसे करें:

```c++
#include <iostream>
#include <curl/curl.h>

// हर अनुरोध के लिए एक नया हैंडलर बनाएं
CURL *curl = curl_easy_init();

// अनुरोध को सेट करें
curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");

// अनुरोध को भेजें
curl_easy_perform(curl);

// हैंडलर को साफ करें
curl_easy_cleanup(curl);
```

आउटपुट:

```
<!doctype html>
<html>
<head>
    <title>उदाहरण वेबसाइट</title>
</head>
<body>
    <h1>नमस्ते,</h1>
    <p>यह एक उदाहरण वेबसाइट है।</p>
</body>
</html>
```

## गहराई विस्तार:

HTTP अनुरोधों को पहले से ही विभिन्न प्रोटोकॉलों विकसित किया गया है, लेकिन C++ में एक आम प्रयोग है। यह अन्य लोकप्रिय लाइब्रेरी विकल्प जैसे कि libcurl और Poco C++ भी है। एक HTTP अनुरोध भेजने के पहले, एक TCP कनेक्शन स्थापित किया जाता है जो प्रतिक्रिया को प्राप्त करने के बाद समाप्त होता है।

## देखें भी:

- [libcurl डॉक्यूमेंटेशन](https://curl.haxx.se/libcurl/)
- [C++ से HTTP से संबंधित सवालों का समाधान](https://stackoverflow.com/questions/tagged/c%2B%2B+http)
- [Poco C++ वेबसाइट](https://pocoproject.org/)
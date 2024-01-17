---
title:                "बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजना"
html_title:           "C++: बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजना"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्‍या और क्यों?

HTTP अनुरोध को बेसिक प्रमाणीकरण के साथ भेजना क्‍या है और प्रोग्रामर्स इसे क्‍यों करते हैं। यह एक प्रभावी तरीका है जिससे आप दूसरे सर्वर से डेटा को सुरक्षित रूप से भेज सकते हैं।

## कैसे करें?

```C++
#include <iostream>
#include <curl/curl.h>

int main() {
    CURL *curl;
    CURLcode res;
    
    curl = curl_easy_init();
    if (curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");
        curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");
        
        res = curl_easy_perform(curl);
        
        if (res != CURLE_OK) {
            std::cout << "An error occurred: " << curl_easy_strerror(res) << std::endl;
        }
        
        curl_easy_cleanup(curl);
    }
    
    return 0;
}
```

आपको सभी सरणियां सेट करने के बाद, आपको अपने HTTP अनुरोध को भेज सकते हैं। अनुरोध सफल होने पर, आपको एक सफलतापूर्ण प्यारी देखने को मिलेगा।

## गहराई में

कोड में दिए गए सारे सेट करने के सिवाए, आपको curl को अपने सिस्‍टम पर स्थापित करना होगा। इतिहास देखने पर, curl को सभी ऑपरेटिंग सिस्‍टमों पर उपलब्ध कराने वाले सर्वर तालिका पर साल 1997 में दर्ज किया गया था। अन्य विकल्पों में स्थानीय क्‍षुधा लाइब्रेरी और OpenSSL के साथ सी से इंप्‍लीमेंट करना शामिल है। सार्वजनिक कुंजी टोटल्स सहित सभी प्रमाणीकरण दस्‍तावेज़ीकरण को पढ़ना आपके लिए उपयोगी हो सकता है।

## और देखें

संबन्धित स्रोतों के लिंक। इसके साथ कुछ जागरूकता और अधिक गहराई।
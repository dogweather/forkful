---
title:                "C++: बेसिक प्रमाणीकरण के साथ संदेश भेजना - कंप्यूटर प्रोग्रामिंग में एक लेख"
simple_title:         "बेसिक प्रमाणीकरण के साथ संदेश भेजना - कंप्यूटर प्रोग्रामिंग में एक लेख"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

वर्तमान दौर में, आज की तारीफ़े और भुगतानी जैसी घटियापूर्ण स्थितियों के चलते बहुत से अनुसंधानात्मक वेब अनुप्रयोगों को अपनी सुरक्षा बढ़ाने के लिए एक सुरक्षित तरीके का आविष्कार करने की आवश्यकता है। HTTP अनुरोध में बेसिक प्रमाणीकरण का उपयोग करके, हम अपने अनुरोध और उत्तर के बीच सुरक्षित संवाद सुनिश्चित कर सकते हैं।

## कैसे करें

```
#include <iostream>
#include <curl/curl.h>
using namespace std;

static size_t writeCallback(void *contents, size_t size, size_t nmemb, void *userp)
{
    ((string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main()
{

    CURL *curl;
    CURLcode res;
    string readBuffer;

    curl_global_init(CURL_GLOBAL_ALL);

    curl = curl_easy_init();
    if (curl)
    {
        // Set URL
        curl_easy_setopt(curl, CURLOPT_URL, "www.example.com");

        // Set basic authentication credentials
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");

        // Set callback function for retrieving response data
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);

        // Perform HTTP request
        res = curl_easy_perform(curl);

        // Check for errors
        if (res != CURLE_OK)
        {
            cout << "Error occurred: " << curl_easy_strerror(res) << endl;
        }

        // Print response
        cout << readBuffer << endl;

        // Clean up
        curl_easy_cleanup(curl);
    }

    return 0;
}
```

इस कोड के द्वारा, हम एक C++ कमांड लाइन अनुप्रयोग में एक HTTP अनुरोध भेज सकते हैं जो बेसिक प्रमाणीकरण की आवश्यकता परिभाषित करता है। हम यह सुनिश्चित करते हैं कि हमारी प्रतिक्रिया सुरक्षित है और हम अपने उपयोगकर्ता नाम और पासवर्ड के साथ अपने सर्वर से संवाद कर सकते हैं।

## गहराई में जाएं

बेसिक प्रमाणीकरण HTTP अनुरोध में एक अत्यंत महत्वपूर्ण तकनीक है जो इंटरनेट सुरक्षा को सुनिश्चित करता है। यह विशेषता भुगतान और
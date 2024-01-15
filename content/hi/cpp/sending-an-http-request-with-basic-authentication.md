---
title:                "HTTP अनुरोध भेजना बेसिक प्रमाणीकरण के साथ"
html_title:           "C++: HTTP अनुरोध भेजना बेसिक प्रमाणीकरण के साथ"
simple_title:         "HTTP अनुरोध भेजना बेसिक प्रमाणीकरण के साथ"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

बेसिक ऑथेंटिकेशन के साथ एचटीटीपी अनुरोध भेजने का काम क्यों किया जाता है। इसका सबसे बड़ा उद्देश्य होता है कि उपभोक्ता के डेटा की सुरक्षा बनाए रखना। जब भी एक उपयोगकर्ता HTTP अनुरोध भेजता है, तो उनके द्वारा भेजे गए डेटा को सुरक्षित रूप से सर्वर तक पहुंचने के लिए उपयोगकर्ता को अपना प्रमाणीकरण सब्जेक्ट और क्रेडेंशियल्स कहने होते हैं। यह सुनिश्चित करता है कि केवल अधिकृत उपयोगकर्ता ही सर्वर से डेटा को प्राप्त कर सकते हैं।

## कैसे करें

```C++
#include <iostream>
#include <curl/curl.h>

// बेसिक ऑथेंटिकेशन के साथ एचटीटीपी अनुरोध भेजने के लिए कोड
int main() {

  // सर्वर URL और क्रेडेंशियल्स को सेट करें
  const std::string server_url = "https://example.com";
  const std::string username = "username";
  const std::string password = "password";

  // CURL इंस्टेंस बनाये और प्रमाणीकरण जोड़ें
  CURL *curl = curl_easy_init();
  if(curl) {

    // प्रमाणीकरण जोड़ने के लिए ऑप्शन सेट करें
    curl_easy_setopt(curl, CURLOPT_USERNAME, username.c_str());
    curl_easy_setopt(curl, CURLOPT_PASSWORD, password.c_str());

    // एचटीटीपी पोस्ट रिक्वेस्ट बनाये
    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json"); // से कोई भी अनुरोध शीर्षक जोड़ें
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);

    // सर्वर से प्रतिसाद को प्राप्त करने के लिए कॉलबैक फंक्शन निर्देशित करें
    curl_easy_setopt(curl, CURLOPT_URL, server_url.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    
    // पोस्ट डेटा सेट करें
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "{\"name\": \"John\", \"age\": 25}");

    // अनुरोध भेजने के लिए कॉल करें
    CURLcode res = curl_easy_perform(curl);
    
    // कॉल कार्रवाई को समाप्त क
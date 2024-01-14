---
title:                "C++: वेब पेज डाउनलोड करना"
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों

वेब पेज डाउनलोड करने के लिए जब कोई प्रोग्रामिंग लैंग्वेज जैसे सी++ का उपयोग करता है तो वे अपने सिस्टम पर विभिन्न डेटा या जानकारियों को ऑनलाइन सुलभ रूप से एक ही स्थान पर शामिल कर सकते हैं।

## कैसे करें

जैसा कि हमने अभी बताया, वेब पेज डाउनलोड करने के लिए सबसे पहले हमें सी++ लैंग्वेज में एक प्रोग्राम लिखना होगा। इसके उदाहरण के लिए हम अपने प्रोग्राम को "webpage_downloader.cpp" नाम से सेव करेंगे।

```C++
#include <iostream>
#include <fstream>
#include <curl/curl.h>

using namespace std;

// वेब पेज को डाउनलोड करने के लिए फंक्शन
int download_webpage(string url, string file_name) {
    // फाइल से डाउनलोडेड डेटा को संचित करने के लिए एक ऑब्जेक्ट बनाएं
    FILE *fp;
    CURL *curl = curl_easy_init();

    // जो लॉकल फाइल पर डेटा संग्रह करेंगे, ना कि मेमोरी में
    // डेटा का अधिक उपयोग करने के लिए
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
    // बाहरी लाइब्रेरी का उपयोग करें जो एक कनेक्शन स्थापित करती है
    if (curl_easy_perform(curl) != CURLE_OK) {
        fprintf(fp, "Unable to connect to %s\n", url.c_str());
        return -1;
    }
    // कनेक्शन बंद करें और फाइल बंद करें
    curl_easy_cleanup(curl);
    fclose(fp);
    return 0;
}

int main() {
    // कोई भी यूआरएल डाउनलोड करें।
    string url = "https://www.google.com/";
    // "index.html" फाइल को डाउनलोड करें
    string file_name = "index.html";

    // फंक्शन को कॉल करें
    int result = download_webpage(url, file_name);

    // अगर कोई समस्या आती है, तो यहां उसे प्रिंट करें
    if (result == -1) {
        cout << "Failed to download webpage." << endl;
        return 1;
    }
    // अन्यथा सफलतापूर्वक डाउन